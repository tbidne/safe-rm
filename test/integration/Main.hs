-- | Entrypoint for integration tests.
--
-- @since 0.1
module Main (main) where

import Integration.Prelude
import Integration.SafeRm qualified as SafeRm
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import Test.Tasty qualified as T
import Test.Tasty qualified as Tasty
import UnliftIO.Directory qualified as Dir

-- | Runs integration tests.
--
-- @since 0.1
main :: IO ()
main =
  T.defaultMain $
    Tasty.withResource setup teardown $ \args ->
      testGroup
        "Integration Tests"
        [ SafeRm.tests args
        ]

setup :: IO FilePath
setup = do
  tmpDir <- (</> "safe-rm/integration") <$> Dir.getTemporaryDirectory

  createDirectoryIfMissing True tmpDir
  pure tmpDir

teardown :: FilePath -> IO ()
teardown tmpDir = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = removePathForcibly tmpDir
    doNothing =
      putStrLn $ "*** Not cleaning up tmp dir: " <> tmpDir
