-- | Entrypoint for unit tests.
--
-- @since 0.1
module Main (main) where

import Functional.Commands.D qualified as D
import Functional.Commands.M qualified as M
import Functional.Commands.R qualified as R
import Functional.Commands.X qualified as X
import Functional.Prelude
import Functional.TestArgs (TestArgs (..))
import System.Directory qualified as Dir
import Test.Tasty qualified as Tasty

-- | Runs unit tests.
--
-- @since 0.1
main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.withResource setup teardown specs

specs :: IO TestArgs -> TestTree
specs args =
  testGroup
    "Functional Tests"
    [ D.tests args,
      X.tests args,
      R.tests args,
      M.tests args
    ]

setup :: IO TestArgs
setup = do
  tmpDir <- (</> "del") <$> Dir.getTemporaryDirectory

  createDirectoryIfMissing False tmpDir
  pure $ MkTestArgs tmpDir

teardown :: TestArgs -> IO ()
teardown args = Dir.removePathForcibly (args ^. #tmpDir)
