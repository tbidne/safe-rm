-- | Configuration Tests.
--
-- @since 0.1
module Config.Toml
  ( tests,
  )
where

import Config.Prelude
import SafeRm.Runner (FinalConfig (trashHome, verbose), getConfiguration)
import System.Environment qualified as SysEnv

-- | @since 0.1
tests :: TestTree
tests =
  testGroup
    "Toml"
    [ parsesExample,
      argsOverridesToml,
      defaultConfig
    ]

parsesExample :: TestTree
parsesExample = testCase "Parses Example" $ do
  finalConfig <- SysEnv.withArgs argList getConfiguration
  Just "./tmp" @=? finalConfig ^. #trashHome
  False @=? finalConfig ^. #verbose
  where
    argList = ["-c", "examples/config.toml", "d", "foo"]

argsOverridesToml :: TestTree
argsOverridesToml = testCase "Args overrides Toml" $ do
  finalConfig <- SysEnv.withArgs argList getConfiguration
  Just "not-tmp" @=? finalConfig ^. #trashHome
  True @=? finalConfig ^. #verbose
  where
    argList =
      [ "-c",
        "examples/config.toml",
        "-t",
        "not-tmp",
        "--verbose",
        "d",
        "foo"
      ]

defaultConfig :: TestTree
defaultConfig = testCase "Default config" $ do
  finalConfig <- SysEnv.withArgs argList getConfiguration
  Nothing @=? finalConfig ^. #trashHome
  False @=? finalConfig ^. #verbose
  where
    argList =
      [ "d",
        "foo"
      ]
