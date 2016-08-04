module GHC.Type.Test.Plugin (plugin) where

import Control.Arrow (second, (***))
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Dynamic hiding (TyCon, mkTyConApp, splitTyConApp, tyConName, mkAppTy)
import Data.Function (on)
import qualified Data.List as List
import Data.Maybe (mapMaybe, fromMaybe, listToMaybe, catMaybes, isJust)
import Data.Monoid ((<>))
import Data.Tagged (Tagged(..))
import GHC.Paths
import System.Environment
import Test.QuickCheck

import Annotations
import DataCon
import DynFlags
import FastString
import GHC hiding (Failed)
import GHC.TcPluginM.Extra (evByFiat, tracePlugin)
import Name
import Outputable (showSDocUnsafe, ppr, Outputable)
import Packages
import Plugins (Plugin (..), defaultPlugin, CommandLineOption)
import Serialized
import TcEvidence (EvTerm)
import TcPluginM
import TcRnTypes (Ct(..), TcPlugin(..), TcPluginResult(..), CtEvidence(..), ctEvidence, ctEvPred)
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
phantheckPlugin options =
  tracePlugin "phantheck" TcPlugin
    { tcPluginInit = initialize options
    , tcPluginSolve = solve
    , tcPluginStop = \_ -> pure ()
    }

justify :: Ct -> Maybe (EvTerm, Ct)
justify ct = (,ct) <$> evMagic ct

solve :: (TyCons, Mode) -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solve (tyCons@TyCons{..}, mode) _ _ wanted =
  pure $
    case mode of
      Lax ->
        let
          addPosts = filter (isJust . simplify (fmap unTagged . checkAddProps tyCons)) wanted
          fromJustNote = fromMaybe (error "Must be @EqPred NomEq@ because @simplifyEqPred@ checks for that")
        in
        TcPluginOk ((fromJustNote . justify) <$> addPosts) []
      Strict tests ->
        let
          (original, simplified) =
            unzip $
              mapMaybe
                (\ct -> (,) <$> justify ct <*> simplify (simplifyPostconditions tyCons tests) ct)
                wanted
        in
        TcPluginOk original simplified

simplify :: (Type -> Maybe Type) -> Ct -> Maybe Ct
simplify = modifyNonCanonicalEvPred . simplifyEqPred . simplifyLeft

simplifyEqPred :: (PredTree -> Maybe PredTree) -> TcPredType -> Maybe TcPredType
simplifyEqPred f =
  eqPredToType <=< f . classifyPredType

simplifyPostconditions :: TyCons -> [(Test, Result)] -> Type -> Maybe Type
simplifyPostconditions tyCons tests =
  fmap (mkList . map (mkStrLitTy . fsLit . unTagged) . postconditions tyCons tests) . checkAddProps tyCons

data TestResult
  = NoPrereq
  | Failed
  | Passed
  deriving (Eq, Show)

postconditions :: TyCons -> [(Test, Result)] -> Tagged "AddProps" Type -> [Tagged "prop" String]
postconditions TyCons{..} tests ty =
  map fst . filter ((== Determinate Passed) . snd) $ postconditionsAndResults
  where
    testsForFunction = filter (\(Test{..}, _) -> fnName == (mkStrLitTy . fsLit <$> functionName)) tests
    fnName = fst . extractAddProps $ ty
    postconditionsAndResults :: [(Tagged "prop" String, PostconditionResult)]
    postconditionsAndResults =
      fmap (second aggregatePostconditionResults) .
      groupOnSplit id .
      map (\tr@(Test{..}, _) -> (postcondition,) . lookupTest ty $ tr) $
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

initialize :: [CommandLineOption] -> TcPluginM (TyCons, Mode)
initialize _ = do
  tyCons <- fromJustNote <$> getPhantheckConstructors
  tcPluginIO (lookupEnv "PHANTHECK_IN_PROGRESS") >>= \case
    Nothing ->
      tcPluginIO . defaultErrorHandler defaultFatalMessager defaultFlushOut . runGhcT (Just GHC.Paths.libdir) $ do
        liftIO $ setEnv "PHANTHECK_IN_PROGRESS" "TRUE"
        _ <- setupPackages
        testMod <- loadModules
        Just modInfo <- getModuleInfo testMod
        setContext [IIModule $ moduleName testMod]
        testDb <- fmap catMaybes . mapM (testAndResult tyCons) $ modInfoExports modInfo
        liftIO $ mapM_ (putStrLn . showTest) testDb
        pure (tyCons, Strict testDb)
    Just _ ->
      pure (tyCons, Lax)
  where
    fromJustNote = fromMaybe (error "Could not find @phantheck@ type constructors")

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
  _ <- setSessionDynFlags flags'
  liftIO $ initPackages flags'


mkTest :: TyCons -> Name -> GhcT IO (Maybe Test)
mkTest tyCons@TyCons{..} nm = do
  AnId export <- fromJustNote <$> lookupName nm
  mAnn <- listToMaybe <$> findGlobalAnns deserializeWithData (NamedTarget nm)
  pure $ do
    (functionName, postcondition) <- (Tagged *** Tagged) <$> mAnn :: Maybe (Tagged "functionName" String, Tagged "prop" String)
    preconditionsTy <- checkProps tyCons =<< findTy props (varType export)
    let
      preconditions = extractProps preconditionsTy
    pure Test{..}
  where
    fromJustNote = fromMaybe (error "We got this @nm@ from @modInfoExports@ so it should definitely be available")

data TyCons
  = TyCons
  { props :: TyCon
  , addProps :: TyCon
  }

show' :: (Outputable a) => a -> String
show' = showSDocUnsafe . ppr

lookupTest :: Tagged "AddProps" Type -> (Test, Result) -> TestResult
lookupTest props (Test{..}, r)
  | (unTagged <$> preconditions) `subSetTy` (unTagged <$> snd (extractAddProps props)) =
    case r of
      Success{} ->
        Passed
      _ ->
        Failed
  | otherwise =
    NoPrereq

checkProps :: TyCons -> Type -> Maybe (Tagged "Props" Type)
checkProps TyCons{..} ty = do
  (tyCon, [propsTy, _val]) <- splitTyConApp_maybe ty
  if tyCon `eqTyCon` props
    then
      Just (Tagged ty) <* elems propsTy
    else
      Nothing

extractProps :: Tagged "Props" Type -> [Tagged "prop" Type]
extractProps (Tagged ty) =
  map Tagged . fromJustNote . elems . head . snd . splitTyConApp $ ty
  where
    fromJustNote =
      fromMaybe
        (error $
           "@Props@ should have had a type-level list. " <>
           "Instead it looks like @" <> show' ty <> "@.")

checkAddProps :: TyCons -> Type -> Maybe (Tagged "AddProps" Type)
checkAddProps TyCons{..} ty = do
  (tyCon, [_fnName, propsTy]) <- splitTyConApp_maybe ty
  if tyCon `eqTyCon` addProps
    then
      Just (Tagged ty) <* elems propsTy
    else
      Nothing

extractAddProps :: Tagged "AddProps" Type -> (Tagged "functionName" Type, [Tagged "prop" Type])
extractAddProps (Tagged ty) =
  (\[fnName, propsTy] -> (Tagged fnName, map Tagged . fromJustNote . elems $ propsTy)) . snd . splitTyConApp $ ty
  where
    fromJustNote =
      fromMaybe
        (error $
           "@AddProps@ should have had a function name and type-level list. " <>
           "Instead it looks like @" <> show' ty <> "@.")

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
      props <- tcLookupTyCon =<< lookupOrig testM (mkTcOcc "Props")
      addProps <- tcLookupTyCon =<< lookupOrig testM (mkTcOcc "AddProps")
      pure . Just $ TyCons{..}
    _ -> pure Nothing

-- | Applies function to @x@ in @x ~ y@
simplifyLeft :: (Type -> Maybe Type) -> PredTree -> Maybe PredTree
simplifyLeft f predTree =
  case predTree of
    EqPred NomEq ty1 ty2 ->
      (\x -> EqPred NomEq x ty2) <$> f ty1
    _ ->
      Nothing

stackDb :: PkgConfRef
stackDb = PkgConfFile "/home/eric/.stack/snapshots/x86_64-linux-nix/lts-5.9/7.10.3/pkgdb"
projDb :: PkgConfRef
projDb = PkgConfFile "/home/eric/code/phantheck/.stack-work/install/x86_64-linux-nix/lts-5.9/7.10.3/pkgdb"

-- * Utils

-- | Recursively splits a type until finding the desired constructor. Then returns the type with that @TyCon@.
-- @findTy Props ((->) (Props '["a", "b"] Int) Int)@ would return @Props '["a", "b"] Int@
findTy :: TyCon -> Type -> Maybe Type
findTy desired ty =
  case splitTyConApp_maybe ty of
    Just (tyCon, apps)
      | tyCon `eqTyCon` desired
      -> Just ty
      | otherwise
      -> listToMaybe $ mapMaybe (findTy desired) apps
    Nothing
      -> Nothing

evMagic :: Ct -> Maybe EvTerm
evMagic ct =
  case classifyPredType . ctEvPred . ctEvidence $ ct of
    EqPred NomEq t1 t2 ->
      Just (evByFiat "phantheck" t1 t2)
    _ ->
      Nothing

subSetTy :: [Type] -> [Type] -> Bool
subSetTy = subSetBy eqType

-- | There's no @Eq@ or @Ord@ for @Type@
subSetBy :: (a -> b -> Bool) -> [a] -> [b] -> Bool
subSetBy f needle haystack = all (\n -> isJust $ List.find (f n) haystack) needle

eqTyCon :: TyCon -> TyCon -> Bool
eqTyCon = (==) `on` (occName . tyConName)

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
elems :: Type -> Maybe [Type]
elems list =
  go list []
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

modifyNonCanonicalEvPred :: (TcPredType -> Maybe TcPredType) -> Ct -> Maybe Ct
modifyNonCanonicalEvPred f (CNonCanonical ev) =
  (\x -> CNonCanonical ev{ctev_pred = x}) <$> f (ctev_pred ev)
modifyNonCanonicalEvPred _ _ = Nothing

-- | Convert a @PredTree@ back into a @PredType@
eqPredToType :: PredTree -> Maybe PredType
eqPredToType (EqPred NomEq ty1 ty2) = Just $ mkEqPred ty1 ty2
eqPredToType _ = Nothing
