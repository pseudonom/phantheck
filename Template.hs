{-# LANGUAGE TupleSections #-}

module Template where

import           Control.Applicative ((<$>))
import           Control.Arrow       ((***))
import           Data.Function       (on)
import           Data.List           (nubBy, stripPrefix)
import           Data.Maybe          (catMaybes)
import           Language.Haskell.TH (Name, Pred, Q, TyVarBndr (PlainTV),
                                      Type (ForallT), TypeQ, appT, conT, equalP,
                                      forallT, mkName, nameBase, promotedConsT,
                                      promotedNilT, promotedT, promotedTupleT,
                                      runIO, varT)
import           Prelude             hiding (pred)

import           Data.List.Split     (splitOn)
import           Text.Regex.PCRE     ((=~))

type Postcondition = TypeQ
type Precondition = TypeQ

-- Adds and preserves properties based on quickcheck functions and preconditions
-- Generates predicate like:
-- props' ~ Postconditions props '['(Ascending, '[]), '(NonNull, '[NonNull])]
checkPost :: FilePath -> Name -> Name -> Name -> Q Pred
checkPost fp fn pres posts = pred =<< raise' . propSets .
                              findPropLines <$> runIO (readFile fp) where
  findPropLines = catMaybes . fmap (stripPrefix ("prop_" ++ nameBase fn ++ "_")) . lines
  propSets :: [String] -> [(Postcondition, [Precondition])]
  propSets = catMaybes . fmap (\(a, b) -> fmap ((promotedT . mkName $ a,)
                                                . precondify) $ extract b) .
             nubBy ((==) `on` fst) . fmap (break (== ' ')) where
    extract = fstMatch . (=~ "'\\[(.*?)\\]") where
      fstMatch :: (String, String, String, [String]) -> Maybe String
      fstMatch (_, _, _, x:_) = Just x
      fstMatch x = Nothing
    precondify :: String -> [Precondition]
    precondify "" = []
    precondify xs = fmap (promotedT . mkName) . splitOn "," $ xs
  raise' :: [(Postcondition, [Precondition])] -> TypeQ
  raise' = raise . fmap (\(post', pres') -> tuple post' (raise pres')) where
    tuple = appT . appT (promotedTupleT 2)
  pred = equalP (varT posts) . appT (appT (conT $ mkName "Postconditions") (varT pres))

addPost :: Name -> TypeQ -> TypeQ
addPost fn t = do
  (ForallT [PlainTV props, PlainTV props'] _ t') <- t
  forallT [PlainTV props, PlainTV props']
    (sequence [checkPost "Demo.hs" fn props props']) (return t')

type TypeList = Type
raise :: [TypeQ] -> Q TypeList
raise = foldr (appT . appT promotedConsT) promotedNilT
