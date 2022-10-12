-- | Entrypoint for functional tests.
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
import SafeRm.Effects.Terminal (Terminal (putStrLn))
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import Test.Tasty qualified as Tasty
import UnliftIO.Directory qualified as Dir

-- | Runs functional tests.
--
-- @since 0.1
main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.withResource setup teardown $ \_ ->
      testGroup
        "Functional Tests"
        [ D.tests,
          X.tests,
          E.tests,
          R.tests,
          L.tests,
          M.tests
        ]

setup :: IO ()
setup = do
  tmpDir <- getTestDir

  createDirectoryIfMissing True tmpDir
  pure ()

teardown :: () -> IO ()
teardown _ = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = getTestDir >>= Dir.removePathForcibly
    doNothing =
      getTestDir >>= \d ->
        putStrLn $ "*** Not cleaning up tmp dir: " <> d
