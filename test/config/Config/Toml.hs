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
  Just (Just InfoS) @=? cfg ^. #consoleLog
  Just (Just DebugS) @=? cfg ^. #fileLog
  where
    argList = ["-c", "examples/config.toml", "d", "foo"]

argsOverridesToml :: TestTree
argsOverridesToml = testCase "Args overrides Toml" $ do
  (cfg, _) <- SysEnv.withArgs argList getConfiguration

  Just "not-tmp" @=? cfg ^. #trashHome
  Just (Just ErrorS) @=? cfg ^. #consoleLog
  Just (Just InfoS) @=? cfg ^. #fileLog
  where
    argList =
      [ "-c",
        "examples/config.toml",
        "-t",
        "not-tmp",
        "--console-log",
        "error",
        "--file-log",
        "info",
        "d",
        "foo"
      ]

defaultConfig :: TestTree
defaultConfig = testCase "Default config" $ do
  (cfg, _) <- SysEnv.withArgs argList getConfiguration

  Nothing @=? cfg ^. #trashHome
  Nothing @=? cfg ^. #consoleLog
  Nothing @=? cfg ^. #fileLog
  where
    argList =
      [ "d",
        "foo"
      ]
