{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import           Control.Applicative ((<$>))
import           Data.List           (sort)

import           Test.QuickCheck     (Arbitrary (arbitrary), getNonEmpty,
                                      quickCheck)

import           Template            (addPost)
import           Type                (Postconditions, Precondition)


data Property = Ascending | NonNull
newtype Propertized (props :: [Property]) a  =
  Propertize { getPropertized :: a } deriving Show


head' :: (Precondition NonNull props) =>
         Propertized props [a] -> a
head' = head . getPropertized

minimum' :: (Precondition Ascending props,
             Precondition NonNull props) =>
            Propertized props [Int] -> Int
minimum' = head'

sort' :: $(addPost 'sort' [t| props' =>
           Propertized props [Int] -> Propertized props' [Int]
         |])
sort' = Propertize . sort . getPropertized


unsorted :: Propertized '[NonNull] [Int]
unsorted = Propertize [2, 3, 1]

foo :: Int
foo = minimum' . sort' $ unsorted

{-
If bar is uncommented, the following error is thrown
during compilation, as hoped:

Couldn't match type ‛'False’ with ‛'True’
Expected type: 'True
  Actual type: Type.Elem 'Ascending '['NonNull]
In the expression: minimum' unsorted
In an equation for ‛bar’: bar = minimum' unsorted
-}
-- bar :: Int
-- bar = minimum' unsorted


instance Arbitrary (Propertized '[] [Int]) where
  arbitrary = Propertize <$> arbitrary
instance Arbitrary (Propertized '[NonNull] [Int]) where
  arbitrary = Propertize . getNonEmpty <$> arbitrary

{-
If prop_sort'_Ascending is commented, the following error is thrown
during compilation, as hoped:

Couldn't match type ‛'False’ with ‛'True’
Expected type: 'True
  Actual type: Type.Elem 'Ascending '['NonNull]
In the first argument of ‛(.)’, namely ‛minimum'’
In the expression: minimum' . sort'
In the expression: minimum' . sort' $ unsorted
-}

-- Sorting adds Ascending
prop_sort'_Ascending :: Propertized '[] [Int] -> Bool
prop_sort'_Ascending (Propertize []) = True
prop_sort'_Ascending xs = fst $ foldr (\c (b, p) -> (b && (c <= p), c))
                          (True, last $ xs') xs' where
  xs' = getPropertized . sort' $ xs

-- Sorting preserves NonNull
prop_sort'_NonNull :: Propertized '[NonNull] [Int] -> Bool
prop_sort'_NonNull xs = not . null . getPropertized . sort' $ xs


main :: IO ()
main = do
  quickCheck prop_sort'_Ascending
  quickCheck prop_sort'_NonNull
