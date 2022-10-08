{-# LANGUAGE OverloadedLists #-}

-- | Configuration Tests.
--
-- @since 0.1
module Config.Toml
  ( tests,
  )
where

import Config.Prelude
import SafeRm.Data.Paths (PathI (MkPathI), (<//>))
import SafeRm.Effects.Logger
  ( LogContext (consoleLogLevel, fileLogLevel, namespace),
    LogLevel (Debug, Error, Info, None),
  )
import SafeRm.Env (Env (fileLogPath, logContext, trashHome))
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
  (env, _) <- SysEnv.withArgs argList getConfiguration

  "./tmp" @=? env ^. #trashHome
  "./tmp/.log" @=? env ^. #fileLogPath
  ["runner"] @=? env ^. (#logContext % #namespace)
  Info @=? env ^. (#logContext % #consoleLogLevel)
  Debug @=? env ^. (#logContext % #fileLogLevel)
  where
    argList = ["-c", "examples/config.toml", "d", "foo"]

argsOverridesToml :: TestTree
argsOverridesToml = testCase "Args overrides Toml" $ do
  (env, _) <- SysEnv.withArgs argList getConfiguration

  "not-tmp" @=? env ^. #trashHome
  "not-tmp/.log" @=? env ^. #fileLogPath
  ["runner"] @=? env ^. (#logContext % #namespace)
  Error @=? env ^. (#logContext % #consoleLogLevel)
  None @=? env ^. (#logContext % #fileLogLevel)
  where
    argList =
      [ "-c",
        "examples/config.toml",
        "-t",
        "not-tmp",
        "--console-log-level",
        "error",
        "--file-log-level",
        "none",
        "d",
        "foo"
      ]

defaultConfig :: TestTree
defaultConfig = testCase "Default config" $ do
  defTrash <- getDefaultTrash
  (env, _) <- SysEnv.withArgs argList getConfiguration

  MkPathI defTrash @=? env ^. #trashHome
  MkPathI defTrash <//> ".log" @=? env ^. #fileLogPath
  ["runner"] @=? env ^. (#logContext % #namespace)
  Error @=? env ^. (#logContext % #consoleLogLevel)
  None @=? env ^. (#logContext % #fileLogLevel)
  where
    argList =
      [ "d",
        "foo"
      ]
