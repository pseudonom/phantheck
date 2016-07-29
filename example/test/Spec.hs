module Main where

import Test.Hspec
import Test.Hspec.QuickCheck

import Lib

{-# ANN foo ("bar" :: String, 2 :: Int)#-}
foo :: String
foo = "zd"

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Lib" $ do
    it "works" $ do
      True `shouldBe` True
    prop "ourAdd is commutative" $ \x ->
      x `shouldBe` ()
