module GHC.Type.Test where

import GHC.TypeLits

newtype Props (ps :: [Symbol]) a = Props {unProps :: a } deriving (Show, Eq, Ord)
type family AddProps (fn :: Symbol) (pres :: [Symbol]) :: [Symbol]
-- type instance AddProps "sort" '["non-empty"] = '["sorted", "non-empty"]
type family Elem (y :: k) (xs :: [k]) :: Bool where
  y `Elem` '[] = 'False
  y `Elem` y ': xs = 'True
  y `Elem` x ': xs = y `Elem` xs
type Precondition a props = (a `Elem` props) ~ 'True
