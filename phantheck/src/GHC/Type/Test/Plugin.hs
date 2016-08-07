module GHC.Type.Test.Plugin (plugin) where

import Control.Arrow (second, (***))
import Control.Lens hiding (Strict, cons)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Distribution.Helper
import Data.Dynamic hiding (TyCon, mkTyConApp, splitTyConApp, tyConName, mkAppTy)
import Data.Function (on)
import qualified Data.List as List
import Data.Maybe (mapMaybe, fromMaybe, listToMaybe, catMaybes, isJust)
import Data.Monoid ((<>))
import Data.Tagged (Tagged(..))
import GHC.Paths
import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Cradle
import Language.Haskell.GhcMod.PathsAndFiles
import Language.Haskell.GhcMod.Stack
import System.Directory
import System.Environment
import System.FilePath
import Test.QuickCheck

import Annotations
import DataCon
import DynFlags
import FastString
import GHC hiding (Failed)
import GHC.TcPluginM.Extra (evByFiat, tracePlugin)
import HscTypes
import Name
import Outputable (showSDocUnsafe, ppr, Outputable)
import Packages
import Plugins (Plugin (..), defaultPlugin, CommandLineOption)
import Serialized
import TcEvidence (EvTerm)
import TcPluginM
import TcRnTypes (Ct(..), TcPlugin(..), TcPluginResult(..), CtEvidence(..), ctEvPred, TcLclEnv(..))
import qualified TcRnTypes
import TcType (TcPredType)
import TyCon
import Type
import TysWiredIn hiding (eqTyCon)
import Var

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = Just . phantheckPlugin }

data Test
  = Test
  { functionName :: Tagged "functionName" String -- ^ Function that test refers to
  , preconditions :: [Tagged "prop" Type] -- ^ Preconditions of test
  , postcondition :: Tagged "prop" String -- ^ Postcondition that test checks
  }

data Mode
  = Lax
  | Strict [(Test, Result)]

phantheckPlugin :: [CommandLineOption] -> TcPlugin
phantheckPlugin [testFile] =
  tracePlugin "phantheck" TcPlugin
    { tcPluginInit = initialize testFile
    , tcPluginSolve = solve
    , tcPluginStop = \_ -> pure ()
    }

justifyAddPosts :: TyCons -> Ct -> Maybe (EvTerm, Ct)
justifyAddPosts tyCons@TyCons{..} ct =
  (\(t1, t2) -> (evByFiat "phantheck" t1 t2, ct) <$ preview (tyConInTree addPropsTc . addPropsOptic tyCons) t1) <=<
  preview nonCanonicalNomEqPred $ ct

solve :: (TyCons, Mode) -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solve (tyCons@TyCons{..}, mode) _ _ wanted =
  pure $
    case mode of
      Lax ->
        TcPluginOk (mapMaybe (justifyAddPosts tyCons) wanted) []
      Strict tests ->
        let
          simplify = traverseOf (nonCanonicalNomEqPred . _1 . tyConInTree addPropsTc) (simplifyPostconditions tyCons tests)
          (original, simplified) = unzip $ mapMaybe (\ct -> (,) <$> justifyAddPosts tyCons ct <*> simplify ct) wanted
        in
        TcPluginOk original simplified

nonCanonicalNomEqPred :: Traversal' Ct (Type, Type)
nonCanonicalNomEqPred = nonCanonicalEvidence . evPred . nomEqPred

simplifyPostconditions :: TyCons -> [(Test, Result)] -> Type -> Maybe Type
simplifyPostconditions tyCons tests =
  fmap (review (tyList typeSymbolKind) . map (mkStrLitTy . fsLit . unTagged) . postconditions tyCons tests) .
  preview (addPropsOptic tyCons)

data TestResult
  = NoPrereq
  | Failed
  | Passed
  deriving (Eq, Show)

postconditions :: TyCons -> [(Test, Result)] -> (Tagged "functionName" Type, [Tagged "prop" Type]) -> [Tagged "prop" String]
postconditions TyCons{..} tests (fnName, props) =
  map fst . filter ((== Determinate Passed) . snd) $ postconditionsAndResults
  where
    testsForFunction = filter (\(Test{..}, _) -> fnName == (mkStrLitTy . fsLit <$> functionName)) tests
    postconditionsAndResults :: [(Tagged "prop" String, PostconditionResult)]
    postconditionsAndResults =
      fmap (second aggregatePostconditionResults) .
      groupOnSplit id .
      map (\tr@(Test{..}, _) -> (postcondition,) . mkTestResult props $ tr) $
      testsForFunction

aggregatePostconditionResults :: [TestResult] -> PostconditionResult
aggregatePostconditionResults = List.foldl' step NoTests
  where
    step Indeterminate _ = Indeterminate
    step (Determinate l) r = maybe Indeterminate Determinate $ l `combineResults` r
    step NoTests r = Determinate r

data PostconditionResult
  = Determinate TestResult
  | Indeterminate
  | NoTests
  deriving (Eq, Show)

combineResults :: TestResult -> TestResult -> Maybe TestResult
combineResults NoPrereq r = Just r
combineResults l NoPrereq = Just l
combineResults l r
  | l == r = Just l
  | otherwise = Nothing -- If we get a success and a failure, it's nonsensical

initialize :: FilePath -> TcPluginM (TyCons, Mode)
initialize testFile = do
  tyCons <- fromJustTyCons <$> getPhantheckConstructors
  tcPluginIO (lookupEnv "PHANTHECK_IN_PROGRESS") >>= \case
    Nothing -> do
      fileBeingChecked <- unpackFS . srcSpanFile . tcl_loc . snd <$> getEnvs
      tcPluginIO . defaultErrorHandler defaultFatalMessager defaultFlushOut . runGhcT (Just GHC.Paths.libdir) $ do
        liftIO $ setEnv "PHANTHECK_IN_PROGRESS" "TRUE"
        _ <- setupPackages fileBeingChecked
        testMod <- loadModules fileBeingChecked testFile
        modInfo <- fromJustModInfo <$> getModuleInfo testMod
        setContext [IIModule $ moduleName testMod]
        testDb <- fmap catMaybes . mapM (testAndResult tyCons) $ modInfoExports modInfo
        liftIO $ mapM_ (putStrLn . showTest) testDb
        pure (tyCons, Strict testDb)
    Just _ ->
      pure (tyCons, Lax)
  where
    fromJustTyCons = fromMaybe (error "Could not find @phantheck@ type constructors")
    fromJustModInfo = fromMaybe (error "Couldn't find module info about test module")

-- | Load required modules and return test module
loadModules :: FilePath -> FilePath -> GhcT IO Module
loadModules fileBeingChecked testFile = do
  lib <- guessTarget fileBeingChecked Nothing
  test <- guessTarget testFile Nothing
  setTargets [lib, test]
  _ <- load LoadAllTargets
  findModule (mkModuleName "Main") Nothing

setupPackages :: FilePath -> GhcT IO (DynFlags, [PackageKey])
setupPackages fileBeingChecked = do
  packages <- liftIO $ findPkgDbs fileBeingChecked
  flags <- getSessionDynFlags
  let
    flags' =
      flags
        { hscTarget = HscInterpreted
        , ghcLink = LinkInMemory
        , extraPkgConfs = const packages
        }
  _ <- setSessionDynFlags flags'
  liftIO $ initPackages flags'


mkTest :: TyCons -> Name -> GhcT IO (Maybe Test)
mkTest tyCons@TyCons{..} nm = do
  AnId export <- fromJustNote <$> lookupName nm
  mAnn <- listToMaybe <$> findGlobalAnns deserializeWithData (NamedTarget nm)
  pure $ do
    (functionName, postcondition) <- (Tagged *** Tagged) <$> mAnn :: Maybe (Tagged "functionName" String, Tagged "prop" String)
    preconditions <- fmap fst . preview (tyConInTree propsTc . propsOptic tyCons) $ varType export
    pure Test{..}
  where
    fromJustNote = fromMaybe (error "We got this @nm@ from @modInfoExports@ so it should definitely be available")

data TyCons
  = TyCons
  { propsTc :: TyCon
  , addPropsTc :: TyCon
  }

show' :: (Outputable a) => a -> String
show' = showSDocUnsafe . ppr

mkTestResult :: [Tagged "prop" Type] -> (Test, Result) -> TestResult
mkTestResult props (Test{..}, r)
  | (unTagged <$> preconditions) `subSetTy` (unTagged <$> props) =
    case r of
      Success{} ->
        Passed
      _ ->
        Failed
  | otherwise =
    NoPrereq

propsOptic :: TyCons -> Prism' Type ([Tagged "prop" Type], Tagged "val" Type)
propsOptic TyCons{..} =
  prism reconstruct deconstruct
    where
      deconstruct ty =
        maybe (Left ty) Right $ do
          (tyCon, [propsTy, val]) <- splitTyConApp_maybe ty
          if tyCon `eqTyCon` propsTc
            then
              (,Tagged val) . map Tagged <$> preview (tyList typeSymbolKind) propsTy
            else
              Nothing
      reconstruct (props, ty) =
        mkTyConApp propsTc [review (tyList typeSymbolKind) $ unTagged <$> props, unTagged ty]

addPropsOptic :: TyCons -> Prism' Type (Tagged "functionName" Type, [Tagged "prop" Type])
addPropsOptic TyCons{..} =
  prism reconstruct deconstruct
    where
      deconstruct ty =
        maybe (Left ty) Right $ do
          (tyCon, [fnName, propsTy]) <- splitTyConApp_maybe ty
          if tyCon `eqTyCon` addPropsTc
            then
              (Tagged fnName,) . map Tagged <$> preview (tyList typeSymbolKind) propsTy
            else
              Nothing
      reconstruct (fnName, props) =
        mkTyConApp addPropsTc [unTagged fnName, review (tyList typeSymbolKind) $ unTagged <$> props]

testAndResult:: TyCons -> Name -> GhcT IO (Maybe (Test, Result))
testAndResult tyCons nm =
  runMaybeT $
    (,) <$>
      MaybeT (mkTest tyCons nm) <*>
      MaybeT (runTest nm)

runTest :: Name -> GhcT IO (Maybe Result)
runTest exportNm =
  traverse liftIO . fromDynamic =<<
    dynCompileExpr (("quickCheckResult " <>) . occNameString . getOccName $ exportNm)

showTest :: (Test, Result) -> String
showTest (Test{..}, result) =
  unTagged functionName <> ": " <> show' (unTagged <$> preconditions) <> " -> " <> unTagged postcondition <> " = " <> passFail
  where
    passFail =
      case result of
        Success{..} -> "Passed"
        _ -> "Failed"

getPhantheckConstructors :: TcPluginM (Maybe TyCons)
getPhantheckConstructors = do
  testR <- findImportedModule (mkModuleName "GHC.Type.Test") (Just $ fsLit "phantheck")
  case testR of
    (Found _ testM) -> do
      propsTc <- tcLookupTyCon =<< lookupOrig testM (mkTcOcc "Props")
      addPropsTc <- tcLookupTyCon =<< lookupOrig testM (mkTcOcc "AddProps")
      pure . Just $ TyCons{..}
    _ -> pure Nothing

-- * Utils

tyConInTree :: TyCon -> Traversal' Type Type
tyConInTree desired f ty =
  case splitTyConApp_maybe ty of
    Just (tyCon, apps)
      | tyCon `eqTyCon` desired
      -> f ty
      | otherwise
      -> mkTyConApp tyCon <$> traverse (tyConInTree desired f) apps
    Nothing
      -> pure ty


subSetTy :: [Type] -> [Type] -> Bool
subSetTy = subSetBy eqType

-- | There's no @Eq@ or @Ord@ for @Type@
subSetBy :: (a -> b -> Bool) -> [a] -> [b] -> Bool
subSetBy f needle haystack = all (\n -> isJust $ List.find (f n) haystack) needle

eqTyCon :: TyCon -> TyCon -> Bool
eqTyCon = (==) `on` (occName . tyConName)

tyList :: Type -> Prism' Type [Type]
tyList kind =
  prism construct deconstruct
    where
      construct =
        List.foldl' (\acc el -> mkAppTy (mkTyConApp cons [kind, el]) acc) (mkTyConApp nil [kind])
          where
            nil = promoteDataCon nilDataCon
            cons = promoteDataCon consDataCon
      deconstruct ty =
        maybe (Left ty) Right $ go ty []
          where
            go list' xs =
              case splitTyConApp_maybe list' of
                Just (_cons, [_kind, x, rest]) ->
                  go rest $ x : xs
                Just (_cons, [_kind]) ->
                  Just xs
                _ ->
                  Nothing

groupOnSplit :: (Ord b) => (a -> (b, c)) -> [a] -> [(b, [c])]
groupOnSplit f =
  map split . List.groupBy ((==) `on` fst . f) . List.sortOn (fst . f)
  where
    split [] = error "Empty list after @groupBy@ is nonsensical"
    split xs@(x:_) = (fst . f $ x, snd . f <$> xs)

evPred :: Lens' CtEvidence PredType
evPred = lens ctev_pred (\ev pred -> ev{ctev_pred = pred})

nonCanonicalEvidence :: Prism' Ct CtEvidence
nonCanonicalEvidence =
  prism CNonCanonical deconstruct
    where
      deconstruct (CNonCanonical ev) = Right ev
      deconstruct ct = Left ct

nomEqPred :: Prism' PredType (Type, Type)
nomEqPred =
  prism (uncurry mkEqPred) deconstruct
    where
      deconstruct pt =
        case classifyPredType pt of
          EqPred NomEq ty1 ty2 -> Right (ty1, ty2)
          _ -> Left pt

findPkgDbs :: (IOish m) => FilePath -> m [PkgConfRef]
findPkgDbs fp = do
  Cradle{..} <- findCradle fp
  (fmap . fmap) pkgDbConvert $ runQuery (defaultQueryEnv cradleRootDir cradleDistDir) packageDbStack
  where
    -- If @ghc-mod@ can't find the @cradle@ there's not much we can do to recover, just @error@
    findCradle = fmap (either (error . show) id . fst) . runGhcModT defaultOptions . findCradle' . takeDirectory

pkgDbConvert :: ChPkgDb -> PkgConfRef
pkgDbConvert ChPkgGlobal = GlobalPkgConf
pkgDbConvert ChPkgUser = UserPkgConf
pkgDbConvert (ChPkgSpecific fp) = PkgConfFile fp

