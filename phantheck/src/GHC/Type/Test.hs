{-# LANGUAGE UndecidableInstances #-}

module GHC.Type.Test where

import GHC.TypeLits
import Language.Haskell.TH (Q)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH (addDependentFile)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>), takeDirectory)

newtype Props (ps :: [Symbol]) a = Props {unProps :: a } deriving (Show, Eq, Ord)
type family AddProps (fn :: Symbol) (pres :: [Symbol]) :: [Symbol]
type family And (a :: Bool) (b :: Bool) :: Bool where
  'True `And` 'True = 'True
  a `And` b = 'False
type family Elem (y :: k) (xs :: [k]) :: Bool where
  y `Elem` '[] = 'False
  y `Elem` y ': xs = 'True
  y `Elem` x ': xs = y `Elem` xs
type family Subset (xs :: [k]) (ys :: [k]) :: Bool where
  '[] `Subset` ys = 'True
  (x ': xs) `Subset` ys = (x `Elem` ys) `And` (xs `Subset` ys)
type Preconditions xs ys = Subset xs ys ~ 'True

addDependentFileRelative :: FilePath -> Q ()
addDependentFileRelative relativeFile = do
  currentFilename <- TH.loc_filename <$> TH.location
  wd <- TH.runIO getCurrentDirectory
  let
    relativePath = takeDirectory (wd </> currentFilename) </> relativeFile
  TH.addDependentFile relativePath
