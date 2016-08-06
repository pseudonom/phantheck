module GHC.Type.Test where

import GHC.TypeLits
import Language.Haskell.TH (Q)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH (addDependentFile)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>), takeDirectory)

newtype Props (ps :: [Symbol]) a = Props {unProps :: a } deriving (Show, Eq, Ord)
type family AddProps (fn :: Symbol) (pres :: [Symbol]) :: [Symbol]
-- type instance AddProps "sort" '["non-empty"] = '["sorted", "non-empty"]
type family Elem (y :: k) (xs :: [k]) :: Bool where
  y `Elem` '[] = 'False
  y `Elem` y ': xs = 'True
  y `Elem` x ': xs = y `Elem` xs
type Precondition a props = (a `Elem` props) ~ 'True

addDependentFileRelative :: FilePath -> Q ()
addDependentFileRelative relativeFile = do
  currentFilename <- TH.loc_filename <$> TH.location
  wd <- TH.runIO getCurrentDirectory
  let
    relativePath = takeDirectory (wd </> currentFilename) </> relativeFile
  TH.addDependentFile relativePath
