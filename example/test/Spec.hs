{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import GHC.Type.Test
import Lib

{-# ANN sort__ascending ("sort", "ascending") #-}
sort__ascending :: Props '[] [Int] -> Bool
sort__ascending xs = ascending . unProps . sort $ xs

{-# ANN sort_nonEmpty_ascending ("sort", "ascending") #-}
sort_nonEmpty_ascending :: Props '["non-empty"] [Int] -> Bool
sort_nonEmpty_ascending xs = ascending . unProps . sort $ xs

{-# ANN sort_nonEmpty_nonEmpty ("sort", "non-empty") #-}
sort_nonEmpty_nonEmpty :: Props '["non-empty"] [Int] -> Bool
sort_nonEmpty_nonEmpty (Props xs) = not $ null xs

ascending :: Ord a => [a] -> Bool
ascending [] = True
ascending xs = and $ zipWith (<=) xs (tail xs)

main :: IO ()
main = hspec spec

instance (Arbitrary a) => Arbitrary (Props '[] a) where
  arbitrary = Props <$> arbitrary
instance (Arbitrary a) => Arbitrary (Props '["non-empty"] [a]) where
  arbitrary = Props . getNonEmpty <$> arbitrary

spec :: Spec
spec =
  describe "Lib" $ do
    prop "sort adds ascending to emptys" $ \(x :: Props '[] [Int]) ->
      sort__ascending x `shouldBe` True
    prop "sort adds ascending to non-emptys" $ \(x :: Props '["non-empty"] [Int]) ->
      sort_nonEmpty_ascending x `shouldBe` True
    prop "sort preserves non-empty" $ \(x :: Props '["non-empty"] [Int]) ->
      sort_nonEmpty_nonEmpty x `shouldBe` True
