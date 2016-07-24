{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}

module Plugin where

-- external
import Control.Arrow       ((***))
import Control.Monad       ((<=<))
import Control.Monad.IO.Class
import Data.Dynamic hiding (TyCon, mkTyConApp)
import Data.List           (partition)
import Data.Maybe          (mapMaybe, fromMaybe)
import Data.Monoid         ((<>))
import GHC
import GHC.Paths
import GHC.TcPluginM.Extra (evByFiat, tracePlugin)
import GHC.TypeLits        (Symbol)
import System.Environment

import Annotations
import ApiAnnotation 
import DynFlags      
import FastString    
import HeaderInfo    
import Lexer         
import Module
import MonadUtils    
import OccName
import Packages
import Parser        
import Serialized
import SrcLoc        
import StringBuffer  
import Var
-- import Language.Haskell.Interpreter

-- GHC API
import FastString (fsLit)
import GHC        ()
import Module     (mkModuleName)
import OccName    (mkTcOcc)
import Outputable (showSDocUnsafe, ppr)
import Plugins    (Plugin (..), defaultPlugin, CommandLineOption)
import TcEvidence (EvTerm)
import TcPluginM  (TcPluginM, tcLookupTyCon, tcPluginIO, lookupOrig, findImportedModule, FindResult(..))
import TcRnTypes  (Ct(..), TcPlugin(..), TcPluginResult (..), CtEvidence(..),
                   ctEvidence, ctEvPred)
import TcType     (TcPredType)
import TyCon      (TyCon)
import "ghc" Type
#if __GLASGOW_HASKELL__ >= 711
import TyCoRep    (Type (..), TyLit (..))
#else
import TypeRep    (Type (..), TyLit (..))
#endif

type family FazzleDazzle (fn :: Symbol) (a :: [k]) :: [k]

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = Just . phantheckPlugin }

phantheckPlugin :: [CommandLineOption] -> TcPlugin
phantheckPlugin options =
  tracePlugin "phantheck" TcPlugin
  { tcPluginInit  = lookupPost options
  , tcPluginSolve = printStuff
  , tcPluginStop  = \_ -> return ()
  }

printStuff :: TyCon -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
printStuff tyCon _ _ wanted = do
  let
    (original, simplified) = unzip $ mapMaybe (\ct -> (ct,) <$> modifyNonCanonicalEvPred (simplifyEqPred tyCon) ct) wanted
    original' = mapMaybe (\ct -> (,ct) <$> evMagic ct) original
  tcPluginIO $ putStrLn $ "TyCon: " <> showSDocUnsafe (ppr tyCon)
  tcPluginIO $ putStrLn $ "Wanted: " <> showSDocUnsafe (ppr wanted)
  tcPluginIO $ putStrLn $ "TyEqs: " <> show (showSDocUnsafe . ppr $ simplified)
  pure (TcPluginOk original' simplified)

simplifyEqPred :: TyCon -> TcPredType -> Maybe TcPredType
simplifyEqPred tyCon =
  eqPredToType <=< simplifyLeft (appMaybe (fixPost tyCon) . appMaybe (fixPre tyCon)) . classifyPredType

modifyNonCanonicalEvPred :: (TcPredType -> Maybe TcPredType) -> Ct -> Maybe Ct
modifyNonCanonicalEvPred f (CNonCanonical ev) =
  (\x -> CNonCanonical $ ev {ctev_pred = x}) <$> f (ctev_pred ev)
modifyNonCanonicalEvPred _ _ = Nothing

appMaybe :: (a -> Maybe a) -> a -> a
appMaybe f a = fromMaybe a $ f a

eqPredToType :: PredTree -> Maybe PredType
eqPredToType (EqPred NomEq ty1 ty2) = Just $ mkEqPred ty1 ty2
eqPredToType _ = Nothing

simplifyLeft :: (Type -> Type) -> PredTree -> Maybe PredTree
simplifyLeft f predTree =
  case predTree of
    EqPred NomEq ty1 ty2 -> Just (EqPred NomEq (f ty1) ty2)
    _ -> Nothing

fixPost :: TyCon -> Type -> Maybe Type
fixPost dazzle ty =
  case splitTyConApp_maybe ty of
    Just (tyCon, [_kind, fnName, props])
      | tyCon == dazzle ->
        Just props
    _ -> Nothing

fixPre :: TyCon -> Type -> Maybe Type
fixPre dazzleCon ty =
  case splitTyConApp_maybe ty of
    Just (elemTyFam, [kind, needle, dazzle]) ->
      case splitTyConApp_maybe dazzle of
        Just (tyCon, [_kind, fnName, props])
          | tyCon == dazzleCon ->
            Just $ mkTyConApp elemTyFam [kind, needle, props]
          -- elems props
        _ -> Nothing
    _ -> Nothing

-- | Transforms type-level list into list of types
elems :: Type -> [Type]
elems list = go list []
  where
    go list' acc =
      case splitTyConApp_maybe list' of
        Just (_con, [_kind, sym, rest]) -> go rest $ sym : acc
        Just (_, [_]) -> acc

evMagic :: Ct -> Maybe EvTerm
evMagic ct =
  case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2 -> Just (evByFiat "foo" t1 t2)
    _ -> Nothing


lookupPost :: [CommandLineOption] -> TcPluginM TyCon
lookupPost _ = do
  tcPluginIO . defaultErrorHandler defaultFatalMessager defaultFlushOut . runGhcT (Just GHC.Paths.libdir) $ do
    let
    flags <- getSessionDynFlags
    let
      flags' =
        flags
          { hscTarget = HscInterpreted
          , ghcLink = LinkInMemory
          , extraPkgConfs = (stackDB :) . extraPkgConfs flags
          }
    setSessionDynFlags flags'
    liftIO $ initPackages flags'
    target <- guessTarget "Foo" Nothing
    addTarget target
    flag <- load LoadAllTargets
    mod <- findModule (mkModuleName "Foo") Nothing
    Just modInfo <- getModuleInfo mod
    let
      [exportNm] = modInfoExports modInfo
    Just (AnId export) <- lookupName exportNm
    liftIO . putStrLn . showSDocUnsafe . ppr $ varType export
    setContext [IIModule $ moduleName mod]
    Just st <- fromDynamic <$> dynCompileExpr (occNameString . getOccName $ exportNm)
    liftIO $ putStrLn st
    [ann] <- findGlobalAnns deserializeWithData (NamedTarget exportNm)
    liftIO . putStrLn $ fst ann
    liftIO . print $ (snd ann :: Int)
    undefined

stackDB = PkgConfFile "/home/eric/.stack/snapshots/x86_64-linux-nix/lts-5.9/7.10.3/pkgdb"

-- lookupPost :: [CommandLineOption] -> TcPluginM TyCon
-- lookupPost [option] = do
--   let plugin = mkModuleName "Main"
--   md <- lookupModule plugin (fsLit "phantheck")
--   tcPluginIO . defaultErrorHandler defaultFatalMessager defaultFlushOut $
--     runGhcT (Just GHC.Paths.libdir) $ do
--       flags <- getSessionDynFlags
--       let flags' =
--             flags
--               { hscTarget = HscInterpreted
--               , ghcLink = LinkInMemory
--               , packageFlags = ExposePackage (PackageArg "ghc-7.10.3") (ModRenaming False []) : packageFlags flags
--               }
--       liftIO $ print . map packageKeyString . snd =<< initPackages flags'
--       liftIO $ print "foo"
--       target <- guessTarget "Demo.hs" Nothing
--       setTargets [target]
--       -- setContext [ IIDecl . simpleImportDecl . mkModuleName $ "Data.List" ]
--       liftIO $ print "baz"
--       flag <- load LoadAllTargets
--       -- liftIO . putStrLn . showSDocUnsafe . ppr $ flag
--       liftIO $ print "bar"
--       -- getModuleInfo md
--   gcdTcNm <- lookupName md (mkTcOcc "FazzleDazzle")
--   tcLookupTyCon gcdTcNm
