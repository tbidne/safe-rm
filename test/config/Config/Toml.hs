{-# LANGUAGE OverloadedLists #-}

-- | Configuration Tests.
--
-- @since 0.1
module Config.Toml
  ( tests,
  )
where

import Config.Prelude
import Data.HashMap.Strict qualified as Map
import Data.List qualified as L
import SafeRm.Data.Paths (PathI (MkPathI), (<//>))
import SafeRm.Effects.Logger.Types
  ( LogContext (namespace, scribes),
    LogLevel (Debug, Error, Info),
    Scribe (logLevel),
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
  scribes @=? scribeInfo (env ^. (#logContext % #scribes))
  where
    argList = ["-c", "examples/config.toml", "d", "foo"]
    scribes = [("console", Info), ("file", Debug)]

argsOverridesToml :: TestTree
argsOverridesToml = testCase "Args overrides Toml" $ do
  (env, _) <- SysEnv.withArgs argList getConfiguration

  "not-tmp" @=? env ^. #trashHome
  "not-tmp/.log" @=? env ^. #fileLogPath
  ["runner"] @=? env ^. (#logContext % #namespace)
  scribes @=? scribeInfo (env ^. (#logContext % #scribes))
  where
    argList =
      [ "-c",
        "examples/config.toml",
        "-t",
        "not-tmp",
        "--console-log-level",
        "error",
        "--file-log-level",
        "info",
        "d",
        "foo"
      ]
    scribes = [("console", Error), ("file", Info)]

defaultConfig :: TestTree
defaultConfig = testCase "Default config" $ do
  defTrash <- getDefaultTrash
  (env, _) <- SysEnv.withArgs argList getConfiguration

  MkPathI defTrash @=? env ^. #trashHome
  MkPathI defTrash <//> ".log" @=? env ^. #fileLogPath
  ["runner"] @=? env ^. (#logContext % #namespace)
  scribes @=? scribeInfo (env ^. (#logContext % #scribes))
  where
    argList =
      [ "d",
        "foo"
      ]
    scribes = [("console", Error)]

scribeInfo :: HashMap Text Scribe -> [(Text, LogLevel)]
scribeInfo = L.sortOn (view _1) . Map.foldMapWithKey f
  where
    f name scribe = [(name, scribe ^. #logLevel)]
