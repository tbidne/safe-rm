-- | Configuration Tests.
--
-- @since 0.1
module Config.Toml
  ( tests,
  )
where

import Config.Prelude
import SafeRm.Runner (FinalConfig (trashHome), getConfiguration)
import System.Environment qualified as SysEnv

-- | @since 0.1
tests :: TestTree
tests =
  testGroup
    "Toml"
    [ parsesExample,
      argsOverridesToml
    ]

parsesExample :: TestTree
parsesExample = testCase "Parses Example" $ do
  finalConfig <- SysEnv.withArgs argList getConfiguration
  Just "./tmp" @=? finalConfig ^. #trashHome
  where
    argList = ["-c", "examples/config.toml", "d", "foo"]

argsOverridesToml :: TestTree
argsOverridesToml = testCase "Args overrides Toml" $ do
  finalConfig <- SysEnv.withArgs argList getConfiguration
  Just "not-tmp" @=? finalConfig ^. #trashHome
  where
    argList = ["-c", "examples/config.toml", "-t", "not-tmp", "d", "foo"]
