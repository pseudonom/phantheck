module Template where

import           Control.Applicative ((<$>))
import           Data.Function       (on)
import           Data.List           (nubBy, stripPrefix)
import           Data.List.Split     (splitOn)
import           Data.Maybe          (catMaybes, fromJust)
import           Language.Haskell.TH (Name, Pred, Q, TyVarBndr (PlainTV),
                                      Type (ForallT), TypeQ, appT, conT, equalP,
                                      forallT, mkName, nameBase, promotedConsT,
                                      promotedNilT, promotedT, promotedTupleT,
                                      runIO, varT)
import           Prelude             hiding (pred)

type Postcondition = TypeQ
type Precondition = TypeQ

-- Adds and preserves properties based on quickcheck functions and preconditions
-- Generates predicate like:
-- props' ~ Postconditions props '['(Ascending, '[]), '(NonNull, '[NonNull])]
checkPost :: Name -> Name -> Name -> Q Pred
checkPost f pres posts = pred =<< raise' . propSets .
                              findPropLines <$> (runIO $ readFile "Demo.hs") where
  pred = equalP (varT posts) . appT (appT (conT $ mkName "Postconditions") (varT pres))
  findPropLines = catMaybes . fmap (stripPrefix ("prop_" ++ nameBase f ++ "_")) . lines
  raise' :: [(Postcondition, [Precondition])] -> TypeQ
  raise' = raise . fmap (\(post', pres') -> tuple post' (raise pres'))
  tuple = appT . appT (promotedTupleT 2)
  propSets :: [String] -> [(Postcondition, [Precondition])]
  propSets = fmap (\(a, b) -> (promotedT . mkName $ a,
                               precondify . extract $ b)) .
             nubBy ((==) `on` fst) . fmap (break (== ' ')) where
    extract = fst . break (== ']') . fromJust .
              stripPrefix " :: Propertized '["
    precondify :: String -> [Precondition]
    precondify "" = []
    precondify xs = fmap (promotedT . mkName) . splitOn "," $ xs

addPost :: Name -> TypeQ -> TypeQ
addPost f t = do
  (ForallT [PlainTV props, PlainTV props'] _ t') <- t
  forallT [PlainTV props, PlainTV props']
    (sequence [checkPost f props props']) (return t')

type TypeList = Type
raise :: [TypeQ] -> Q TypeList
raise = foldr (appT . (appT promotedConsT)) promotedNilT
