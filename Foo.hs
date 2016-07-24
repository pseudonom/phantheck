{-# LANGUAGE FlexibleContexts #-}
module Foo where

import Data.Monoid
import Data.List
import Data.Text

-- bar :: Int
-- bar = 1

{-# ANN exporting ("bar" :: String, 2 :: Int)#-}
exporting :: (Ord String) => String
exporting = "ab" <> "cd"
