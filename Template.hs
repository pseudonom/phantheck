module Template where

import           Control.Monad
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Language.Haskell.TH
import           System.Environment  (getEnv)

import           Data.List.Split     (splitOn, chunksOf)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as M
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Safe                (atMay)
import           System.Process      (readProcessWithExitCode)
import           Test.QuickCheck     hiding (Property)
import           Text.Regex.PCRE

import           Type                (Postconditions)

type PropName = String
type FunctionName = String
type Postcondition = String
type Preconditions = [String]

-- Adds and preserves properties based on quickcheck functions and preconditions
-- Generates predicate like:
-- `posts ~ Postconditions pres '['(Ascending, '[]), '(NonNull, '[NonNull])]`
checkPost :: Name -> Name -> [(Preconditions, Postcondition)] -> TypeQ
checkPost presName postsName = mkPredicate . reqsAndPostsToType
  where
    reqsAndPostsToType = typesToList . map (\(pres, post) -> [t| '($(preType pres), $(stringToType post)) |])
      where
        preType = typesToList . map stringToType
    mkPredicate reqsAndPosts = runQ [t| $(varT postsName) ~ Postconditions $(varT presName) $(reqsAndPosts) |]

addPost :: TypeQ -> TypeQ
addPost t = do
  fnLineNum <- pred . fst . loc_start <$> location
  fnLine <- (!! fnLineNum) . lines <$> runIO (readFile "Demo.hs")
  let fnName = takeWhile (/= ' ') fnLine
  tests <- getWrittenTests
  passingTests <- do
    beStrict <- runIO $ read <$> getEnv "STRICT"
    if beStrict
      then runTests
      else return . S.fromList . map (mkPropName fnName . snd) $ tests M.! fnName
  (ForallT [PlainTV props, PlainTV props'] _ t') <- t
  forallT [PlainTV props, PlainTV props']
    (sequence [checkPost props props' $ passingPostsAndReqs fnName tests passingTests]) (return t')

passingPostsAndReqs
  :: FunctionName -> Map FunctionName [(Preconditions, Postcondition)] -> Set PropName -> [(Preconditions, Postcondition)]
passingPostsAndReqs fnName tests passingTests =
  filter ((`S.member` passingTests) . mkPropName fnName . snd) $ tests M.! fnName

mkPropName :: FunctionName -> Postcondition -> PropName
mkPropName fnName post = "prop_" <> fnName <> "_" <> post

stringToType :: String -> TypeQ
stringToType = pure . LitT . StrTyLit

-- Promotes a list of types into a type level list
typesToList :: [TypeQ] -> TypeQ
typesToList = foldr (appT . appT promotedConsT) promotedNilT

quickCheckWithName :: ExpQ -> ExpQ
quickCheckWithName expQ = do
  (VarE name) <- expQ
  let name' = nameBase name
  [|putStrLn name' >> quickCheck $expQ|]

getWrittenTests :: Q (Map FunctionName [(Preconditions, Postcondition)])
getWrittenTests =
  M.fromListWith (<>) . mapMaybe (parse <=< stripPrefix "prop_") . lines  <$> runIO (readFile "Demo.hs")
  where
    parse line = (fnName,) . pure . (, post) <$> pres
      where
        (fnName, rest) = break (== '_') line
        (_:post, rest') = break (== ' ') rest
        pres = fmap (splitOnNonEmpty ",") . (`atMay` 1) . getAllTextSubmatches $ rest' =~ "'\\[ *'?(.*?)\\]"
          where
            splitOnNonEmpty _ "" = []
            splitOnNonEmpty f xs = splitOn f xs

runTests :: Q (Set PropName)
runTests = do
  (_, testOutput, _) <- runIO $ readProcessWithExitCode "stack" ["exec", "phantheck"] ""
  runIO $ print testOutput
  return . S.fromList . mapMaybe parseTests . chunksOf 2 $ lines testOutput
  where
    parseTests [propName, "+++ OK, passed 100 tests."] = Just propName
    parseTests _ = Nothing
