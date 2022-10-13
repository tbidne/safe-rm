{-# LANGUAGE OverloadedLists #-}

-- | Configuration Tests.
--
-- @since 0.1
module Config.Toml
  ( tests,
  )
where

import Config.Prelude
import SafeRm.Runner (getConfiguration)
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
  (cfg, _) <- SysEnv.withArgs argList getConfiguration

  Just "./tmp" @=? cfg ^. #trashHome
  Just (Just LevelInfo) @=? cfg ^. #logLevel
  where
    argList = ["-c", "examples/config.toml", "d", "foo"]

argsOverridesToml :: TestTree
argsOverridesToml = testCase "Args overrides Toml" $ do
  (cfg, _) <- SysEnv.withArgs argList getConfiguration

  Just "not-tmp" @=? cfg ^. #trashHome
  Just (Just LevelError) @=? cfg ^. #logLevel
  where
    argList =
      [ "-c",
        "examples/config.toml",
        "-t",
        "not-tmp",
        "--log-level",
        "error",
        "d",
        "foo"
      ]

defaultConfig :: TestTree
defaultConfig = testCase "Default config" $ do
  (cfg, _) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Nothing @=? cfg ^. #logLevel
  where
    argList =
      [ "d",
        "foo",
        "-c",
        "none"
      ]
