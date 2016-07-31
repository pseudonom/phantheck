{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}

module GHC.Type.Test.Plugin (plugin) where

-- external
import Control.Arrow (second)
import Control.Monad       ((<=<))
import Control.Monad.IO.Class
import Data.Dynamic hiding (TyCon, mkTyConApp, splitTyConApp, tyConName, mkAppTy)
import Data.Function (on)
import qualified Data.List as List
import Data.Maybe          (mapMaybe, fromMaybe, listToMaybe, catMaybes, isJust)
import Data.Monoid         ((<>))
import Data.Tagged
import Debug.Trace
import GHC
import GHC.Paths
import GHC.TcPluginM.Extra (evByFiat, tracePlugin)
import System.Environment
import Test.QuickCheck

import Annotations
import DataCon
import DynFlags
import FastString
import Name
import OccName
import Packages
import Serialized
import Var

import GHC        ()
import Outputable (showSDocUnsafe, ppr, Outputable)
import Plugins    (Plugin (..), defaultPlugin, CommandLineOption)
import TcEvidence (EvTerm)
import TcPluginM
import TcRnTypes  (Ct(..), TcPlugin(..), TcPluginResult (..), CtEvidence(..),
                   ctEvidence, ctEvPred)
import TcType     (TcPredType)
import TyCon
import Type
import TysWiredIn

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = Just . phantheckPlugin }

data Test
  = Test
  { functionName :: String -- ^ Function that test refers to
  , preconditions :: [Type] -- ^ Preconditions of test
  , postcondition :: String -- ^ Postcondition that test checks
  }

data Mode
  = Lax
  | Strict [(Test, Result)]

phantheckPlugin :: [CommandLineOption] -> TcPlugin
phantheckPlugin options =
  tracePlugin "phantheck" $ TcPlugin
    { tcPluginInit  = lookupPost options
    , tcPluginSolve = printStuff
    , tcPluginStop  = \_ -> return ()
    }

printStuff :: (TyCons, Mode) -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
printStuff (tyCons@TyCons{..}, mode) _ _ wanted = do
  let
    showMode =
      \case
        Lax -> "Lax"
        Strict _ -> "Strict"
  tcPluginIO $ putStrLn $ showMode mode <> " Wanted: " <> show' wanted
  let
    (original, simplified) = unzip $ mapMaybe (\ct -> (ct,) <$> modifyNonCanonicalEvPred (Just) ct) wanted
    original' = mapMaybe (\ct -> (,ct) <$> evMagic ct) original
  case mode of
    Lax ->
      pure (TcPluginOk original' [])
    Strict tests -> do
      let
        (original, simplified) = unzip $ mapMaybe (\ct -> (ct,) <$> modifyNonCanonicalEvPred (\x -> trace "mazzle" $ simplifyEqPred tyCons tests x) ct) wanted
        original' = mapMaybe (\ct -> (,ct) <$> evMagic ct) original
      tcPluginIO $ putStrLn $ show' simplified
      pure (TcPluginOk original' simplified)

simplifyEqPred :: TyCons -> [(Test, Result)] -> TcPredType -> Maybe TcPredType
simplifyEqPred tyCons tests =
  -- eqPredToType <=< simplifyLeft (appMaybe (fixPost tyCon) . appMaybe (fixPre tyCon)) . classifyPredType
  eqPredToType <=< simplifyLeft (appMaybe ((\x -> trace (show' x) x) . fixPost tyCons tests)) . classifyPredType

modifyNonCanonicalEvPred :: (TcPredType -> Maybe TcPredType) -> Ct -> Maybe Ct
modifyNonCanonicalEvPred f (CNonCanonical ev) =
  (\x -> CNonCanonical $ ev {ctev_pred = x}) <$> f (ctev_pred ev)
modifyNonCanonicalEvPred _ _ = Nothing

appMaybe :: (a -> Maybe a) -> a -> a
appMaybe f a = fromMaybe a $ f a

eqPredToType :: PredTree -> Maybe PredType
eqPredToType (EqPred NomEq ty1 ty2) = Just $ mkEqPred ty1 ty2
eqPredToType _ = Nothing

-- | Applies function to @x@ in @x ~ y@
simplifyLeft :: (Type -> Type) -> PredTree -> Maybe PredTree
simplifyLeft f predTree =
  case predTree of
    EqPred NomEq ty1 ty2 -> Just (EqPred NomEq (f ty1) ty2)
    _ -> Nothing

data Lol
  = NoPrereq
  | TestFail
  | Passed
  deriving (Eq, Show)

fixPost :: TyCons -> [(Test, Result)] -> Type -> Maybe Type
fixPost tyCons@TyCons{..} tests ty = (\x -> trace (show' x) x) . trace (show' ty) $
  case splitTyConApp_maybe ty of
    Just (tyCon, [fnName, props])
      | trace (show' (tyCon, addProps)) $ tyCon == addProps
      ->
        let
          x = map (\a@(Test{..}, _) -> (postcondition,) . reql fnName props $ a ) . filter (\(Test{..}, _) -> fnName == (mkStrLitTy . fsLit $ functionName)) $ tests
          y = groupOnSplit id x
          z = second qqy <$> y
        in
        trace "razzle" $! Just $! non $! map (mkStrLitTy . fsLit . fst) . filter ((== OneTest Passed) . snd) $! z
      | otherwise
      -> trace "fazzle" $! Nothing
    _ -> trace "dazzle" $! Nothing

groupOnSplit f =
  map ls . List.groupBy ((==) `on` fst . f) . List.sortOn (fst . f)
  where
    ls [] = error "Empty list after `groupBy` is nonsensical"
    ls xs@(x:_) = (fst . f $ x, snd . f <$> xs)

reql fnName props (Test{..}, r)
  | preconditions `subSetTy` elems props =
    case r of
      Success{..} -> Passed
      _ -> TestFail
  | otherwise =
    NoPrereq

qqy :: [Lol] -> Boo
qqy = List.foldl' ttl NoTests

data Boo
  = OneTest Lol
  | NoTests
  | TooMany
  deriving (Eq, Show)

ttl TooMany _ = TooMany
ttl (OneTest l) r = OneTest r -- maybe TooMany OneTest $ l `vvo` r
ttl NoTests l = OneTest l

vvo NoPrereq NoPrereq = Just NoPrereq
vvo NoPrereq TestFail = Just TestFail
vvo NoPrereq Passed = Just Passed
vvo TestFail NoPrereq = Just TestFail
vvo TestFail TestFail = Just TestFail
vvo TestFail Passed = Nothing
vvo Passed NoPrereq = Just Passed
vvo Passed TestFail = Nothing
vvo Passed Passed = Nothing

subSetTy = subSetBy eqType
subSetBy f needle haystack = all (\n -> isJust $ List.find (f n) haystack) needle

-- fixPre :: TyCon -> Type -> Maybe Type
-- fixPre dazzleCon ty =
--   case splitTyConApp_maybe ty of
--     Just (elemTyFam, [kind, needle, dazzle]) ->
--       case splitTyConApp_maybe dazzle of
--         Just (tyCon, [_kind, fnName, props])
--           | tyCon == dazzleCon ->
--             Just $ mkTyConApp elemTyFam [kind, needle, props]
--           -- elems props
--         _ -> Nothing
--     _ -> Nothing

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

lookupPost :: [CommandLineOption] -> TcPluginM (TyCons, Mode)
lookupPost _ = do
  propsConstructor <- fromMaybe (error "Could not find `phantheck` type constructor `Props`") <$> getPropsConstructor
  tcPluginIO (lookupEnv "HAPPENING") >>= \case
    Nothing -> do
      tcPluginIO . defaultErrorHandler defaultFatalMessager defaultFlushOut . runGhcT (Just GHC.Paths.libdir) $ do
        liftIO $ setEnv "HAPPENING" "IT'S"
        setupPackages
        mod <- loadModules
        Just modInfo <- getModuleInfo mod
        setContext [IIModule $ moduleName mod]
        bar' <- foo (props propsConstructor) modInfo
        -- Just (AnId export) <- lookupName exportNm
        -- let
        --   ty = varType export
        -- Just st <- fromDynamic <$> dynCompileExpr (occNameString . getOccName $ exportNm) :: GhcT IO (Maybe String)
        -- anns <- findGlobalAnns deserializeWithData (NamedTarget exportNm) :: GhcT IO [(String, Int)]
        pure (propsConstructor, Strict bar')
    Just _ ->
      pure (propsConstructor, Lax)

loadModules = do
  lib <- guessTarget "src/Lib.hs" Nothing
  test <- guessTarget "test/Spec.hs" Nothing
  setTargets [lib, test]
  flag <- load LoadAllTargets
  findModule (mkModuleName "Main") Nothing

setupPackages = do
  flags <- getSessionDynFlags
  let
    flags' =
      flags
        { hscTarget = HscInterpreted
        , ghcLink = LinkInMemory
        , extraPkgConfs = (projDB :) . (stackDB :) . extraPkgConfs flags
        }
  setSessionDynFlags $ List.foldl' xopt_set flags' [Opt_DataKinds, Opt_PolyKinds, Opt_ScopedTypeVariables, Opt_FlexibleInstances, Opt_KindSignatures]
  liftIO $ initPackages flags'

show' :: (Outputable a) => a -> String
show' = showSDocUnsafe . ppr

cole :: TyCon -> ModuleInfo -> GhcT IO [Test]
cole tyCon modInfo = fmap catMaybes . mapM (stup tyCon) $ modInfoExports modInfo

foo :: TyCon -> ModuleInfo -> GhcT IO [(Test, Result)]
foo tyCon modInfo =
  fmap catMaybes . mapM (crip tyCon) $ modInfoExports modInfo
  where
    exportNms = modInfoExports modInfo
    crip tyCon nm = do
      oo <- stup tyCon nm
      case oo of
        Nothing -> pure Nothing
        Just t -> Just . (t,) <$> loon nm

loon :: Name -> GhcT IO Result
loon exportNm = do
  Just st <- fromDynamic <$> dynCompileExpr (("quickCheckResult " <>) . occNameString . getOccName $ exportNm) :: GhcT IO (Maybe (IO Result))
  liftIO st

stup tyCon nm = do
  Just (AnId export) <- lookupName nm
  mAnn <- listToMaybe <$> findGlobalAnns deserializeWithData (NamedTarget nm)
  case mAnn of
    Just (functionName, postcondition) -> do
      case findTy tyCon (varType export) of
        Just preconditions' -> do
          let
            preconditions = propsElems preconditions'
          -- liftIO . putStrLn $ show' $ splitAppTys $ mkTyConApp (promoteDataCon nilDataCon) [typeSymbolKind]
          -- liftIO . putStrLn $ show' $ splitAppTys $ mkPromotedListTy typeSymbolKind
          -- liftIO . putStrLn . show' . splitAppTys . head . snd . splitAppTys $ preconditions'
          pure . Just $ Test{..}
        Nothing -> pure Nothing
    Nothing ->
      pure Nothing

propsElems :: Type -> [Type]
propsElems = elems . head . tail . snd . splitTyConApp

non :: [Type] -> Type
non ts = (\x -> trace (show' x) x) $ List.foldl' (\acc el -> mkAppTy (mkTyConApp cons' [typeSymbolKind, el]) acc) (mkTyConApp nil' [typeSymbolKind]) ts

nil' = promoteDataCon nilDataCon
cons' = promoteDataCon consDataCon

-- | Transforms type-level list into list of types
elems :: Type -> [Type]
elems list = go list []
  where
    go list' acc =
      case splitTyConApp_maybe list' of
        Just (_con, [_kind, sym, rest]) -> go rest $ sym : acc
        Just (_con, [_]) -> acc

stripForAll :: Type -> Type
stripForAll = snd . splitForAllTys

findTy :: TyCon -> Type -> Maybe Type
findTy desired ty =
  case splitTyConApp_maybe . snd $ splitForAllTys ty of
    Just (tyCon, apps)
      | tyCon `tyConEq` desired
      -> Just ty
      | otherwise
      -> listToMaybe $ mapMaybe (findTy desired) apps
    Nothing
      -> Nothing

tyConEq :: TyCon -> TyCon -> Bool
tyConEq = (==) `on` (occName . tyConName)

data TyCons
  = TyCons
  { props :: TyCon
  , addProps :: TyCon
  }

getPropsConstructor :: TcPluginM (Maybe TyCons)
getPropsConstructor = do
  testR <- findImportedModule (mkModuleName "GHC.Type.Test") (Just $ fsLit "phantheck")
  case testR of
    (Found _ testM) -> do
      props <- tcLookupTyCon =<< lookupOrig testM (mkTcOcc "Props")
      addProps <- tcLookupTyCon =<< lookupOrig testM (mkTcOcc "AddProps")
      pure . Just $ TyCons{..}
    _ -> pure Nothing

stackDB = PkgConfFile "/home/eric/.stack/snapshots/x86_64-linux-nix/lts-5.9/7.10.3/pkgdb"
projDB = PkgConfFile "/home/eric/code/phantheck/.stack-work/install/x86_64-linux-nix/lts-5.9/7.10.3/pkgdb"


evMagic :: Ct -> Maybe EvTerm
evMagic ct =
  case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2 -> Just (evByFiat "phantheck" t1 t2)
    _ -> Nothing
