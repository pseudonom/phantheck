{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin GHC.Type.Test.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt GHC.Type.Test.Plugin:test/Spec.hs #-}

module Lib where

import Prelude hiding (min)
import qualified Data.List as List
import Language.Haskell.TH

import GHC.Type.Test

$(addDependentFileRelative "../test/Spec.hs" >> pure [])

min :: Props '["non-empty", "ascending"] [a] -> a
min (Props xs) = head xs

sort :: (Ord a) => Props pre [a] -> Props (AddProps "sort" pre) [a]
sort (Props xs) = Props . List.sort $ xs

sortedList :: Props '["non-empty", "ascending"] [Int]
sortedList = Props [1, 2, 3]

unsortedList :: Props '["non-empty"] [Int]
unsortedList = Props [3, 2, 1]

int1 :: Int
int1 = min sortedList

int2 :: Int
int2 = min $ sort unsortedList
