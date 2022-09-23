-- | Entrypoint for unit tests.
--
-- @since 0.1
module Main (main) where

import Functional.Commands.D qualified as D
import Functional.Prelude
import Functional.TestArgs (TestArgs (..))
-- import Functional.Pythia.Services.Battery qualified as Battery

import System.Directory qualified as Dir
import System.Environment.Guard
import Test.Tasty qualified as Tasty

-- | Runs unit tests.
--
-- @since 0.1
main :: IO ()
main =
  guardOrElse'
    "RUN_FUNCTIONAL"
    ExpectEnvSet
    tests
    (putStrLn "*** Functional tests disabled ***")
  where
    tests =
      Tasty.defaultMain $
        Tasty.withResource setup teardown specs

specs :: IO TestArgs -> TestTree
specs args =
  testGroup
    "Functional Tests"
    [ D.tests args
    ]

setup :: IO TestArgs
setup = do
  tmpDir <- (</> "del") <$> Dir.getTemporaryDirectory

  createDirectoryIfMissing False tmpDir
  pure $ MkTestArgs tmpDir

teardown :: TestArgs -> IO ()
teardown args = Dir.removePathForcibly (args ^. #tmpDir)
