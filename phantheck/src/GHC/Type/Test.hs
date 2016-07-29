module GHC.Type.Test where

import GHC.TypeLits

newtype Props (ps :: [k]) a = Props {unProps :: a } deriving (Show, Eq, Ord)
type family AddProps (fn :: Symbol) (pres :: [k]) :: [k]
-- type instance AddProps "sort" '["non-empty"] = '["sorted", "non-empty"]
