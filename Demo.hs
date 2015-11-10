module Main where

import           Prelude             hiding (head, minimum)
import qualified Data.List           as L

import           Test.QuickCheck     (Arbitrary (arbitrary), getNonEmpty, quickCheck)

import           Template            (addPost)
import           Type                (Postconditions, Precondition)

data Property = Ascending | NonNull

-- A phantom type wrapper which allows us to add properties to another type
newtype Propertized (props :: [Property]) a  =
  Propertize { getPropertized :: a } deriving Show


-- `head` will only operate on lists which have the `NonNull` property
head :: (Precondition 'NonNull props) =>
         Propertized props [a] -> a
head = L.head . getPropertized

-- minimum will only operate on lists which have the `NonNull` and `Ascending` properties
minimum :: (Precondition 'Ascending props,
             Precondition 'NonNull props) =>
            Propertized props [Int] -> Int
minimum = head

-- `sort` has no preconditions,
-- `addPost` will add the `Ascending` property due to `prop_sort_Ascending`
sort :: $(addPost 'sort [t| forall pres posts.
           Propertized pres [Int] -> Propertized posts [Int]
         |])
sort = Propertize . L.sort . getPropertized


-- A `NonNull` and unsorted list
unsorted :: Propertized '[ 'NonNull] [Int]
unsorted = Propertize [2, 3, 1]

-- Typechecks because `sort` adds `Ascending` to unsorted before passing it on to `minimum`
foo :: Int
foo = minimum . sort $ unsorted


-- Fails to typecheck because `unsorted` does not have the `Ascending` property that `minimum` requires
{- The error is as follows:

Couldn't match type ‛'False’ with ‛'True’
Expected type: 'True
  Actual type: Type.Elem 'Ascending '['NonNull]
In the expression: minimum unsorted
In an equation for ‛bar’: bar = minimum unsorted
-}
-- bar :: Int
-- bar = minimum unsorted


instance Arbitrary (Propertized '[] [Int]) where
  arbitrary = Propertize <$> arbitrary
instance Arbitrary (Propertized '[ 'NonNull] [Int]) where
  arbitrary = Propertize . getNonEmpty <$> arbitrary

-- Property verifiers must follow the naming convention `prop_fnName_PropName`.
-- `PropName` specifies the postcondition that the function checks.
-- Properties specified in the type signature are preconditional requirements
-- which must be satisfied for the postcondition to hold.

{-
If `prop_sort_Ascending` is commented, the following error is thrown
during compilation, as hoped:

Couldn't match type ‛'False’ with ‛'True’
Expected type: 'True
  Actual type: Type.Elem 'Ascending '['NonNull]
In the first argument of ‛(.)’, namely ‛minimum’
In the expression: minimum . sort
In the expression: minimum . sort $ unsorted
-}

-- Sorting adds `Ascending`
prop_sort_Ascending :: Propertized '[] [Int] -> Bool
prop_sort_Ascending (Propertize []) = True
prop_sort_Ascending xs = 
  and $ zipWith (<=) xs' (tail xs') where
    xs' = getPropertized . sort $ xs

-- Sorting preserves `NonNull`
prop_sort_NonNull :: Propertized '[ 'NonNull] [Int] -> Bool
prop_sort_NonNull = not . null . getPropertized . sort


main :: IO ()
main = do
  quickCheck prop_sort_Ascending
  quickCheck prop_sort_NonNull
