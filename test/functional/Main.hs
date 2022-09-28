-- | Entrypoint for unit tests.
--
-- @since 0.1
module Main (main) where

import Functional.Commands.D qualified as D
import Functional.Commands.E qualified as E
import Functional.Commands.L qualified as L
import Functional.Commands.M qualified as M
import Functional.Commands.R qualified as R
import Functional.Commands.X qualified as X
import Functional.Prelude
import Functional.TestArgs (TestArgs (..))
import System.Directory qualified as Dir
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import Test.Tasty qualified as Tasty

-- | Runs unit tests.
--
-- @since 0.1
main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.withResource setup teardown $ \args ->
      testGroup
        "Functional Tests"
        [ D.tests args,
          X.tests args,
          E.tests args,
          R.tests args,
          L.tests args,
          M.tests args
        ]

setup :: IO TestArgs
setup = do
  tmpDir <- (</> "del") <$> Dir.getTemporaryDirectory

  createDirectoryIfMissing False tmpDir
  pure $ MkTestArgs tmpDir

teardown :: TestArgs -> IO ()
teardown args = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = Dir.removePathForcibly (args ^. #tmpDir)
    doNothing =
      putStrLn $ "*** Not cleaning up tmp dir: " <> args ^. #tmpDir
