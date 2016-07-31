module GHC.Type.Test where

import GHC.TypeLits

newtype Props (ps :: [Symbol]) a = Props {unProps :: a } deriving (Show, Eq, Ord)
type family AddProps (fn :: Symbol) (pres :: [Symbol]) :: [Symbol]
-- type instance AddProps "sort" '["non-empty"] = '["sorted", "non-empty"]
