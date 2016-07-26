{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}

module GHC.Type.Test.Plugin (plugin) where

-- external
import Control.Monad       ((<=<))
import Control.Monad.IO.Class
import Data.Dynamic hiding (TyCon, mkTyConApp)
import Data.Maybe          (mapMaybe, fromMaybe)
import Data.Monoid         ((<>))
import GHC
import GHC.Paths
import GHC.TcPluginM.Extra (evByFiat, tracePlugin)
import System.Environment

import Annotations
import DynFlags
import OccName
import Packages
import Serialized
import Var

import GHC        ()
import Outputable (showSDocUnsafe, ppr)
import Plugins    (Plugin (..), defaultPlugin, CommandLineOption)
import TcEvidence (EvTerm)
import TcPluginM
import TcRnTypes  (Ct(..), TcPlugin(..), TcPluginResult (..), CtEvidence(..),
                   ctEvidence, ctEvPred)
import TcType     (TcPredType)
import Type

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = Just . phantheckPlugin }

phantheckPlugin :: [CommandLineOption] -> TcPlugin
phantheckPlugin options =
  tracePlugin "phantheck" $ TcPlugin
    { tcPluginInit  = lookupPost options
    , tcPluginSolve = printStuff
    , tcPluginStop  = \_ -> return ()
    }

printStuff :: () -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
printStuff () _ _ wanted = do
  let
  tcPluginIO $ putStrLn $ "Wanted: " <> showSDocUnsafe (ppr wanted)
  pure (TcPluginOk [] [])

lookupPost :: [CommandLineOption] -> TcPluginM ()
lookupPost _ = do
  tcPluginIO . defaultErrorHandler defaultFatalMessager defaultFlushOut . runGhcT (Just GHC.Paths.libdir) $
    liftIO (lookupEnv "HAPPENING") >>= \case
      Just _ -> pure ()
      Nothing -> do
        liftIO $ setEnv "HAPPENING" "IT'S"
        flags <- getSessionDynFlags
        let
          flags' =
            flags
              { hscTarget = HscInterpreted
              , ghcLink = LinkInMemory
              , extraPkgConfs = (projDB :) . (stackDB :) . extraPkgConfs flags
              }
        setSessionDynFlags flags'
        liftIO $ initPackages flags'
        lib <- guessTarget "src/Lib.hs" Nothing
        test <- guessTarget "test/Spec.hs" Nothing
        setTargets [lib, test]
        flag <- load LoadAllTargets
        mod <- findModule (mkModuleName "Main") Nothing
        Just modInfo <- getModuleInfo mod
        let
          [exportNm, _, _] = modInfoExports modInfo
        Just (AnId export) <- lookupName exportNm
        let
          ty = varType export
        setContext [IIModule $ moduleName mod]
        Just st <- fromDynamic <$> dynCompileExpr (occNameString . getOccName $ exportNm) :: GhcT IO (Maybe String)
        anns <- findGlobalAnns deserializeWithData (NamedTarget exportNm) :: GhcT IO [(String, Int)]
        pure ()

stackDB = PkgConfFile "/home/eric/.stack/snapshots/x86_64-linux-nix/lts-5.9/7.10.3/pkgdb"
projDB = PkgConfFile "/home/eric/code/phantheck/.stack-work/install/x86_64-linux-nix/lts-5.9/7.10.3/pkgdb"
