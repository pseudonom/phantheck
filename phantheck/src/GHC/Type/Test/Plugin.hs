{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}

module GHC.Type.Test.Plugin (plugin) where

-- external
import Control.Monad       ((<=<))
import Control.Monad.IO.Class
import Data.Dynamic hiding (TyCon, mkTyConApp)
import qualified Data.List as List
import Data.Maybe          (mapMaybe, fromMaybe)
import Data.Monoid         ((<>))
import GHC
import GHC.Paths
import GHC.TcPluginM.Extra (evByFiat, tracePlugin)
import System.Environment

import Annotations
import DynFlags
import FastString
import OccName
import Packages
import Serialized
import Var

import GHC        ()
import Outputable (showSDocUnsafe, ppr)
import Plugins    (Plugin (..), defaultPlugin, CommandLineOption)
import TcEvidence (EvTerm)
import TcPluginM
import TcRnTypes  (Ct(..), TcPlugin(..), TcPluginResult (..), CtEvidence(..),
                   ctEvidence, ctEvPred)
import TcType     (TcPredType)
import Type

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = Just . phantheckPlugin }

phantheckPlugin :: [CommandLineOption] -> TcPlugin
phantheckPlugin options =
  tracePlugin "phantheck" $ TcPlugin
    { tcPluginInit  = lookupPost options
    , tcPluginSolve = printStuff
    , tcPluginStop  = \_ -> return ()
    }

printStuff :: TyCon -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
printStuff _ _ _ wanted = do
  tcPluginIO $ putStrLn $ "Wanted: " <> showSDocUnsafe (ppr wanted)
  pure (TcPluginOk [] [])

lookupPost :: [CommandLineOption] -> TcPluginM TyCon
lookupPost _ = do
  tcPluginIO . defaultErrorHandler defaultFatalMessager defaultFlushOut . runGhcT (Just GHC.Paths.libdir) $
    liftIO (lookupEnv "HAPPENING") >>= \case
      Just _ -> pure ()
      Nothing -> do
        liftIO $ setEnv "HAPPENING" "IT'S"
        flags <- getSessionDynFlags
        let
          flags' =
            flags
              { hscTarget = HscInterpreted
              , ghcLink = LinkInMemory
              , extraPkgConfs = (projDB :) . (stackDB :) . extraPkgConfs flags
              }
        setSessionDynFlags $ List.foldl' xopt_set flags' [Opt_DataKinds, Opt_PolyKinds]
        liftIO $ initPackages flags'
        lib <- guessTarget "src/Lib.hs" Nothing
        test <- guessTarget "test/Spec.hs" Nothing
        setTargets [lib, test]
        flag <- load LoadAllTargets
        mod <- findModule (mkModuleName "Main") Nothing
        Just modInfo <- getModuleInfo mod
        let
          [exportNm, _, _] = modInfoExports modInfo
        Just (AnId export) <- lookupName exportNm
        let
          ty = varType export
        setContext [IIModule $ moduleName mod]
        Just st <- fromDynamic <$> dynCompileExpr (occNameString . getOccName $ exportNm) :: GhcT IO (Maybe String)
        anns <- findGlobalAnns deserializeWithData (NamedTarget exportNm) :: GhcT IO [(String, Int)]
        pure ()
  fromMaybe (error "Could not find `phantheck` type constructor `Props`") <$> getPropsConstructor

getPropsConstructor :: TcPluginM (Maybe TyCon)
getPropsConstructor =
  findImportedModule (mkModuleName "GHC.Type.Test") (Just $ fsLit "phantheck") >>=
  \case
    Found _ mod -> fmap Just . tcLookupTyCon =<< lookupOrig mod (mkTcOcc "Props")
    _ -> pure Nothing

stackDB = PkgConfFile "/home/eric/.stack/snapshots/x86_64-linux-nix/lts-5.9/7.10.3/pkgdb"
projDB = PkgConfFile "/home/eric/code/phantheck/.stack-work/install/x86_64-linux-nix/lts-5.9/7.10.3/pkgdb"
