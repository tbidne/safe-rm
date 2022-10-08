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
import SafeRm.Effects.Logger (LogContext (logLevel, namespace), LogLevel (Info, None))
import SafeRm.Env (Env (logContext, logPath, trashHome, verbose))
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
  False @=? env ^. #verbose
  "./tmp/.log" @=? env ^. #logPath
  ["runner"] @=? env ^. (#logContext % #namespace)
  Info @=? env ^. (#logContext % #logLevel)
  where
    argList = ["-c", "examples/config.toml", "d", "foo"]

argsOverridesToml :: TestTree
argsOverridesToml = testCase "Args overrides Toml" $ do
  (env, _) <- SysEnv.withArgs argList getConfiguration

  "not-tmp" @=? env ^. #trashHome
  True @=? env ^. #verbose
  "not-tmp/.log" @=? env ^. #logPath
  ["runner"] @=? env ^. (#logContext % #namespace)
  None @=? env ^. (#logContext % #logLevel)
  where
    argList =
      [ "-c",
        "examples/config.toml",
        "-t",
        "not-tmp",
        "--verbose",
        "--log-level",
        "none",
        "d",
        "foo"
      ]

defaultConfig :: TestTree
defaultConfig = testCase "Default config" $ do
  defTrash <- getDefaultTrash
  (env, _) <- SysEnv.withArgs argList getConfiguration

  MkPathI defTrash @=? env ^. #trashHome
  False @=? env ^. #verbose
  MkPathI defTrash <//> ".log" @=? env ^. #logPath
  ["runner"] @=? env ^. (#logContext % #namespace)
  None @=? env ^. (#logContext % #logLevel)
  where
    argList =
      [ "d",
        "foo"
      ]
