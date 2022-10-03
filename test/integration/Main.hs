-- | Entrypoint for integration tests.
--
-- @since 0.1
module Main (main) where

import Integration.MaxRuns (MaxRuns)
import Integration.Prelude
import Integration.SafeRm qualified as SafeRm
import SafeRm.Effects.Terminal (Terminal (putStrLn))
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import Test.Tasty qualified as T
import Test.Tasty qualified as Tasty
import Test.Tasty.Options (OptionDescription (Option))
import UnliftIO.Directory qualified as Dir

-- | Runs integration tests.
--
-- @since 0.1
main :: IO ()
main = do
  let options = T.includingOptions [Option @MaxRuns Proxy]
      ingredients = options : T.defaultIngredients
  T.defaultMainWithIngredients ingredients $
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
    cleanup = Dir.removePathForcibly tmpDir
    doNothing =
      putStrLn $ "*** Not cleaning up tmp dir: " <> tmpDir
