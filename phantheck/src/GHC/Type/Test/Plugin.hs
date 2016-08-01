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
import TysWiredIn hiding (eqTyCon)

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
  tracePlugin "phantheck" TcPlugin
    { tcPluginInit = initialize options
    , tcPluginSolve = solve
    , tcPluginStop = \_ -> return ()
    }

solve :: (TyCons, Mode) -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solve (tyCons@TyCons{..}, mode) _ _ wanted = do
  let
  case mode of
    Lax -> do
      let
        (original, _) = unzip $ mapMaybe (\ct -> (ct,) <$> modifyNonCanonicalEvPred Just ct) wanted
        original' = mapMaybe (\ct -> (,ct) <$> evMagic ct) original
      pure (TcPluginOk original' [])
    Strict tests -> do
      let
        (original, simplified) = unzip $ mapMaybe (\ct -> (ct,) <$> modifyNonCanonicalEvPred (simplifyEqPred tyCons tests) ct) wanted
        original' = mapMaybe (\ct -> (,ct) <$> evMagic ct) original
      tcPluginIO $ putStrLn $ "Wanted: " <> show' wanted
      tcPluginIO $ putStrLn $ show' simplified
      pure (TcPluginOk original' simplified)

simplifyEqPred :: TyCons -> [(Test, Result)] -> TcPredType -> Maybe TcPredType
simplifyEqPred tyCons tests =
  eqPredToType <=< simplifyLeft (appMaybe (fixPost tyCons tests)) . classifyPredType

modifyNonCanonicalEvPred :: (TcPredType -> Maybe TcPredType) -> Ct -> Maybe Ct
modifyNonCanonicalEvPred f (CNonCanonical ev) =
  (\x -> CNonCanonical ev{ctev_pred = x}) <$> f (ctev_pred ev)
modifyNonCanonicalEvPred _ _ = Nothing

appMaybe :: (a -> Maybe a) -> a -> a
appMaybe f a = fromMaybe a $ f a

-- | Convert a @PredTree@ back into a @PredType@
eqPredToType :: PredTree -> Maybe PredType
eqPredToType (EqPred NomEq ty1 ty2) = Just $ mkEqPred ty1 ty2
eqPredToType _ = Nothing

-- | Applies function to @x@ in @x ~ y@
simplifyLeft :: (Type -> Type) -> PredTree -> Maybe PredTree
simplifyLeft f predTree =
  case predTree of
    EqPred NomEq ty1 ty2 -> Just (EqPred NomEq (f ty1) ty2)
    _ -> Nothing

data TestResult
  = NoPrereq
  | TestFail
  | Passed
  deriving (Eq, Show)

fixPost :: TyCons -> [(Test, Result)] -> Type -> Maybe Type
fixPost TyCons{..} tests ty =
  case splitTyConApp_maybe ty of
    Just (tyCon, [fnName, propsTy])
      | tyCon == addProps
      ->
        let
          testsForFunction = filter (\(Test{..}, _) -> fnName == (mkStrLitTy . fsLit $ functionName)) tests
          postconditions :: [(String, PostConditionResult)]
          postconditions =
            fmap (second aggregatePostConditionResults) .
            groupOnSplit id .
            map (\a@(Test{..}, _) -> (postcondition,) . lookupTest propsTy $ a) $
            testsForFunction
        in
        Just . mkList . map (mkStrLitTy . fsLit . fst) . filter ((== OneTest Passed) . snd) $ postconditions
      | otherwise
      -> Nothing
    _ -> Nothing

groupOnSplit :: (Ord b) => (a -> (b, c)) -> [a] -> [(b, [c])]
groupOnSplit f =
  map split . List.groupBy ((==) `on` fst . f) . List.sortOn (fst . f)
  where
    split [] = error "Empty list after `groupBy` is nonsensical"
    split xs@(x:_) = (fst . f $ x, snd . f <$> xs)

lookupTest :: Type -> (Test, Result) -> TestResult
lookupTest props (Test{..}, r)
  | preconditions `subSetTy` elems props =
    case r of
      Success{..} -> Passed
      _ -> TestFail
  | otherwise =
    NoPrereq

aggregatePostConditionResults :: [TestResult] -> PostConditionResult
aggregatePostConditionResults = List.foldl' step NoTests
  where
    step TooMany _ = TooMany
    step (OneTest _) r = OneTest r -- maybe TooMany OneTest $ l `combineResults` r
    -- THIS IS WRONG; FIX IT
    -- It needs to use @combineResults@ but also work for multiple sets of prereqs.
    step NoTests l = OneTest l

data PostConditionResult
  = OneTest TestResult
  | NoTests
  | TooMany
  deriving (Eq, Show)

-- combineResults :: TestResult -> TestResult -> Maybe TestResult
-- combineResults NoPrereq NoPrereq = Just NoPrereq
-- combineResults NoPrereq TestFail = Just TestFail
-- combineResults NoPrereq Passed = Just Passed
-- combineResults TestFail NoPrereq = Just TestFail
-- combineResults TestFail TestFail = Just TestFail
-- combineResults TestFail Passed = Nothing
-- combineResults Passed NoPrereq = Just Passed
-- combineResults Passed TestFail = Nothing
-- combineResults Passed Passed = Nothing

subSetTy :: [Type] -> [Type] -> Bool
subSetTy = subSetBy eqType

subSetBy :: (a -> b -> Bool) -> [a] -> [b] -> Bool
subSetBy f needle haystack = all (\n -> isJust $ List.find (f n) haystack) needle

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

initialize :: [CommandLineOption] -> TcPluginM (TyCons, Mode)
initialize _ = do
  propsConstructor <- fromMaybe (error "Could not find `phantheck` type constructor `Props`") <$> getPhantheckConstructors
  tcPluginIO (lookupEnv "HAPPENING") >>= \case
    Nothing ->
      tcPluginIO . defaultErrorHandler defaultFatalMessager defaultFlushOut . runGhcT (Just GHC.Paths.libdir) $ do
        liftIO $ setEnv "HAPPENING" "IT'S"
        _ <- setupPackages
        testMod <- loadModules
        Just modInfo <- getModuleInfo testMod
        setContext [IIModule $ moduleName testMod]
        testDb <- gatherTestResults (props propsConstructor) modInfo
        liftIO $ mapM_ (putStrLn . showTest) testDb
        pure (propsConstructor, Strict testDb)
    Just _ ->
      pure (propsConstructor, Lax)

showTest :: (Test, Result) -> String
showTest (Test{..}, result) =
  functionName <> ": " <> show' preconditions <> " -> " <> postcondition <> " = " <> passFail
  where
    passFail =
      case result of
        Success{..} -> "Passed"
        _ -> "Failed"

-- | Load required modules and return test module
loadModules :: GhcT IO Module
loadModules = do
  lib <- guessTarget "src/Lib.hs" Nothing
  test <- guessTarget "test/Spec.hs" Nothing
  setTargets [lib, test]
  _ <- load LoadAllTargets
  findModule (mkModuleName "Main") Nothing

setupPackages :: GhcT IO (DynFlags, [PackageKey])
setupPackages = do
  flags <- getSessionDynFlags
  let
    flags' =
      flags
        { hscTarget = HscInterpreted
        , ghcLink = LinkInMemory
        , extraPkgConfs = (projDb :) . (stackDb :) . extraPkgConfs flags
        }
  _ <-
    setSessionDynFlags $
      List.foldl'
        xopt_set flags'
        [ Opt_DataKinds
        , Opt_PolyKinds
        , Opt_ScopedTypeVariables
        , Opt_FlexibleInstances
        , Opt_KindSignatures
        , Opt_TypeOperators
        , Opt_TypeFamilies
        ]
  liftIO $ initPackages flags'

show' :: (Outputable a) => a -> String
show' = showSDocUnsafe . ppr

gatherTestResults :: TyCon -> ModuleInfo -> GhcT IO [(Test, Result)]
gatherTestResults propsCon modInfo =
  fmap catMaybes . mapM testAndResult $ modInfoExports modInfo
  where
    testAndResult nm = do
      mTest <- mkTest propsCon nm
      case mTest of
        Nothing -> pure Nothing
        Just test -> (test,) <$$> runTest nm

runTest :: Name -> GhcT IO (Maybe Result)
runTest exportNm = do
  mTestAction <- fromDynamic <$> dynCompileExpr (("quickCheckResult " <>) . occNameString . getOccName $ exportNm)
  case mTestAction of
    Just testAction -> Just <$> liftIO testAction
    Nothing -> pure Nothing

mkTest :: TyCon -> Name -> GhcT IO (Maybe Test)
mkTest propsCon nm = do
  Just (AnId export) <- lookupName nm
  mAnn <- listToMaybe <$> findGlobalAnns deserializeWithData (NamedTarget nm)
  case mAnn of
    Just (functionName, postcondition) ->
      case findTy propsCon (varType export) of
        Just preconditionTy -> do
          let
            preconditions = propsElems preconditionTy
          pure . Just $ Test{..}
        Nothing -> pure Nothing
    Nothing ->
      pure Nothing

-- | Takes a type like @Props '["a", "b"] Int@ to a list of types like @["a", "b"]@
propsElems :: Type -> [Type]
propsElems = elems . head . snd . splitTyConApp

-- | Turn a list of types into a type-level list
mkList :: [Type] -> Type
mkList = List.foldl' (\acc el -> mkAppTy (mkTyConApp cons [typeSymbolKind, el]) acc) (mkTyConApp nil [typeSymbolKind])

-- | '[]
nil :: TyCon
nil = promoteDataCon nilDataCon
-- | ':
cons :: TyCon
cons = promoteDataCon consDataCon

-- | Transforms type-level list into list of types
elems :: Type -> [Type]
elems list = go list []
  where
    go list' acc =
      case splitTyConApp_maybe list' of
        Just (_con, [_kind, sym, rest]) -> go rest $ sym : acc
        Just (_con, [_kind]) -> acc
        _ -> error "FIX THIS"

-- | Recursively splits a type until finding the desired constructor. Then returns the type with that @TyCon@.
-- @findTy Props (forall x. (->) (Props '["a", "b"] Int) Int)@ would return @Props '["a", "b"] Int@
findTy :: TyCon -> Type -> Maybe Type
findTy desired ty =
  case splitTyConApp_maybe . snd $ splitForAllTys ty of
    Just (tyCon, apps)
      | tyCon `eqTyCon` desired
      -> Just ty
      | otherwise
      -> listToMaybe $ mapMaybe (findTy desired) apps
    Nothing
      -> Nothing

eqTyCon :: TyCon -> TyCon -> Bool
eqTyCon = (==) `on` (occName . tyConName)

data TyCons
  = TyCons
  { props :: TyCon
  , addProps :: TyCon
  }

getPhantheckConstructors :: TcPluginM (Maybe TyCons)
getPhantheckConstructors = do
  testR <- findImportedModule (mkModuleName "GHC.Type.Test") (Just $ fsLit "phantheck")
  case testR of
    (Found _ testM) -> do
      props <- tcLookupTyCon =<< lookupOrig testM (mkTcOcc "Props")
      addProps <- tcLookupTyCon =<< lookupOrig testM (mkTcOcc "AddProps")
      pure . Just $ TyCons{..}
    _ -> pure Nothing

stackDb :: PkgConfRef
stackDb = PkgConfFile "/home/eric/.stack/snapshots/x86_64-linux-nix/lts-5.9/7.10.3/pkgdb"
projDb :: PkgConfRef
projDb = PkgConfFile "/home/eric/code/phantheck/.stack-work/install/x86_64-linux-nix/lts-5.9/7.10.3/pkgdb"

evMagic :: Ct -> Maybe EvTerm
evMagic ct =
  case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2 -> Just (evByFiat "phantheck" t1 t2)
    _ -> Nothing
