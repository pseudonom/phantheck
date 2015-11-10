module Template where

import           Data.Function
import           Data.List
import           Data.Maybe
import           Language.Haskell.TH

import           Data.List.Split     (splitOn)
import           Safe                (atMay)
import           Text.Regex.PCRE

import           Type                (Postconditions)

type Postcondition = String
type Preconditions = [String]

-- Adds and preserves properties based on quickcheck functions and preconditions
-- Generates predicate like:
-- `posts ~ Postconditions pres '['(Ascending, '[]), '(NonNull, '[NonNull])]`
checkPost :: FilePath -> Name -> Name -> Name -> TypeQ
checkPost fp fn presName postsName =
  mkPredicate . reqsAndPostsToType . mkReqsAndPosts . findPropLines =<< runIO (readFile fp)
  where
    findPropLines = catMaybes . map (stripPrefix ("prop_" ++ nameBase fn ++ "_")) . lines
    mkReqsAndPosts :: [String] -> [(Preconditions, Postcondition)]
    mkReqsAndPosts = catMaybes . map (parse . break (== ' '))
      where
        parse (post, pres) = (,post) . precondify <$> extractProps pres
          where
            extractProps = (`atMay` 1) . getAllTextSubmatches . (=~ "'\\[ *'?(.*?)\\]")
            precondify "" = []
            precondify xs = splitOn "," $ xs
    reqsAndPostsToType = typesToList . map (\(pres, post) -> [t| '($(preType pres), $(stringToType post)) |])
      where
        preType = typesToList . map stringToType
    mkPredicate reqsAndPosts = runQ [t| $(varT postsName) ~ Postconditions $(varT presName) $(reqsAndPosts) |]

addPost :: TypeQ -> TypeQ
addPost t = do
  fnLineNum <- pred . fst <$> loc_start <$> location
  fnLine <- (!! fnLineNum) . lines <$> runIO (readFile "Demo.hs")
  let fnName = mkName . fst . break (== ' ') $ fnLine
  (ForallT [PlainTV props, PlainTV props'] _ t') <- t
  forallT [PlainTV props, PlainTV props']
    (sequence [checkPost "Demo.hs" fnName props props']) (return t')

stringToType :: String -> TypeQ
stringToType = promotedT . mkName

-- Promotes a list of types into a type level list
typesToList :: [TypeQ] -> TypeQ
typesToList = foldr (appT . appT promotedConsT) promotedNilT
