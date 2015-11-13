module Main where

import           Prelude             hiding (head, minimum)
import qualified Data.List           as L

import           Test.QuickCheck     hiding (Property)

import           Template            (addPost, quickCheckWithName)
import           Type                (Precondition)

data Property = Ascending | NonNull

-- A phantom type wrapper which allows us to add properties to another type
newtype Prop (props :: [Property]) a  = Prop { unProp :: a } deriving Show


-- `head` will only operate on lists which have the `NonNull` property
head :: (Precondition 'NonNull props) => Prop props [a] -> a
head = L.head . unProp

-- minimum will only operate on lists which have the `NonNull` and `Ascending` properties
minimum :: (Precondition 'Ascending props, Precondition 'NonNull props) => Prop props [Int] -> Int
minimum = head

-- `sort` has no preconditions,
-- `addPost` will add the `Ascending` postcondition due to `prop_sort_Ascending`
sort :: $(addPost [t| forall pres posts.
          Prop pres [Int] -> Prop posts [Int]
        |])
sort = Prop . L.sort . unProp

-- A `NonNull` and unsorted list
unsorted :: Prop '[ 'NonNull] [Int]
unsorted = Prop [2, 3, 1]

-- Typechecks because `sort` adds `Ascending` to unsorted before passing it on to `minimum`
min :: Int
min = minimum . sort $ unsorted

empty :: Prop '[] [Int]
empty = Prop []

sortedEmpty :: Prop '[ 'Ascending] [Int]
sortedEmpty = sort empty

-- The following fails to typecheck because `unsorted` does not have the `Ascending` property that `minimum` requires
{- The error is as follows:

Couldn't match type ‛'False’ with ‛'True’
Expected type: 'True
  Actual type: Type.Elem 'Ascending '['NonNull]
In the expression: minimum unsorted
In an equation for ‛bar’: bar = minimum unsorted
-}
-- bar :: Int
-- bar = minimum unsorted

-- If we define `sort` as follows, our program will fail to compile during the `strict` phase.
-- This is because our `Ascending` test will fail and so `sort` won't add the `Ascending` property `minimum` requires.
-- sort :: $(addPost [t| forall pres posts.
--           Prop pres [Int] -> Prop posts [Int]
--         |])
-- sort = Prop . id . unProp

instance Arbitrary (Prop '[] [Int]) where
  arbitrary = Prop <$> arbitrary
instance Arbitrary (Prop '[ 'NonNull] [Int]) where
  arbitrary = Prop . getNonEmpty <$> arbitrary

-- Property verifiers must follow the naming convention `prop_fnName_PropName`.
-- `PropName` specifies the postcondition that the function checks.
-- Properties specified in the type signature are preconditional requirements
-- which must be satisfied for the postcondition to hold.

{-
If `prop_sort_Ascending` is commented, the following error is thrown
during the non-strict phase of compilation, as hoped:

Couldn't match type ‛'False’ with ‛'True’
Expected type: 'True
  Actual type: Type.Elem 'Ascending '['NonNull]
In the first argument of ‛(.)’, namely ‛minimum’
In the expression: minimum . sort
In the expression: minimum . sort $ unsorted
-}

-- Sorting adds `Ascending`
prop_sort_Ascending :: Prop '[] [Int] -> Bool
prop_sort_Ascending (Prop []) = True
prop_sort_Ascending xs =
  and $ zipWith (<=) xs' (tail xs') where
    xs' = unProp . sort $ xs

-- Sorting preserves `NonNull`
prop_sort_NonNull :: Prop '[ 'NonNull] [Int] -> Bool
prop_sort_NonNull = not . null . unProp . sort


main :: IO ()
main = do
  $(quickCheckWithName [|prop_sort_Ascending|])
  $(quickCheckWithName [|prop_sort_NonNull|])
