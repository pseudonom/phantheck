-- {-# OPTIONS_GHC -fplugin GHC.Type.Test.Plugin #-}

module Lib where

import Prelude hiding (min)
import qualified Data.List as List

import GHC.Type.Test

min :: Props '["sorted", "non-empty"] [a] -> a
min (Props xs) = head xs

sort :: Props pre [Int] -> Props (AddProps "sort" pre) [Int]
sort (Props xs) = Props . List.sort $ xs

sortedList :: Props '["sorted", "non-empty"] [Int]
sortedList = Props [1, 2, 3]

unsortedList :: Props '["non-empty"] [Int]
unsortedList = Props [3, 2, 1]

int1 :: Int
int1 = min sortedList

int2 :: Int
int2 = min $ sort unsortedList
