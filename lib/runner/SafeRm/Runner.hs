{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This modules provides an executable for running safe-rm.
--
-- @since 0.1
module SafeRm.Runner
  ( -- * Main functions
    runSafeRm,
    withEnv,

    -- * Helpers
    getConfiguration,
  )
where

import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.List.NonEmpty qualified as NE
import Data.Text.Encoding qualified as TEnc
import SafeRm qualified
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex (TrashHome, TrashLog),
    (<//>),
  )
import SafeRm.Effects.Logger qualified as Logger
import SafeRm.Effects.Logger.Format
import SafeRm.Effects.Logger.Format qualified as Format
import SafeRm.Effects.Logger.Types
  ( LogContext (MkLogContext, namespace, scribes),
    LogLevel (Error),
    Logger,
    Scribe (MkScribe, logLevel, logger),
  )
import SafeRm.Effects.Terminal (Terminal, putTextLn)
import SafeRm.Env
  ( Env
      ( MkEnv,
        fileLogPath,
        logContext,
        trashHome
      ),
    HasTrashHome,
  )
import SafeRm.Exceptions
  ( ExceptionI (MkExceptionI),
    ExceptionIndex (TomlDecode),
  )
import SafeRm.Prelude
import SafeRm.Runner.Args (Args (command, tomlConfigPath), getArgs)
import SafeRm.Runner.Command
  ( Command
      ( Delete,
        DeletePerm,
        Empty,
        List,
        Metadata,
        Restore
      ),
  )
import SafeRm.Runner.SafeRmT (usingSafeRmT)
import SafeRm.Runner.Toml
  ( TomlConfig
      ( consoleLogLevel,
        fileLogLevel,
        trashHome
      ),
    mergeConfigs,
  )
import System.Exit (ExitCode (ExitSuccess))
import TOML qualified
import UnliftIO.Directory (XdgDirectory (XdgConfig))
import UnliftIO.Directory qualified as Dir

-- | Reads CLI args, optional Toml config, and runs SafeRm.
--
-- @since 0.1
runSafeRm :: (MonadUnliftIO m, Terminal m) => m ()
runSafeRm = do
  -- Combine args and toml config to get final versions of shared config
  -- values. Right now, only the trash home is shared.
  (env, cmd) <- getConfiguration

  usingSafeRmT env $ do
    runCmd cmd
      `catch` doNothingOnSuccess
      `catchAny` handleEx
  where
    runCmd = \case
      Delete paths -> SafeRm.delete (listToSet paths)
      DeletePerm force paths ->
        SafeRm.deletePermanently force (listToSet paths)
      Empty force -> SafeRm.emptyTrash force
      Restore paths -> SafeRm.restore (listToSet paths)
      List -> printIndex *> printMetadata
      Metadata -> printMetadata

    doNothingOnSuccess ExitSuccess = pure ()
    doNothingOnSuccess ex = throwIO ex

    handleEx ex = do
      -- REVIEW: is this good enough?
      -- Logger.localContext consoleLoggingOff $
      $(Logger.logExceptionTH Error) ex
      throwIO ex

-- consoleLoggingOff :: LogContext -> LogContext
-- consoleLoggingOff =
--  over' #scribes (Map.filterWithKey (\k _ -> k /= "console"))

-- | Retrieves the configuration and runs the param function.
--
-- @since 0.1
withEnv :: MonadIO m => ((Env, Command) -> m a) -> m a
withEnv = (>>=) getConfiguration

-- | Parses CLI 'Args' and optional 'TomlConfig' to produce the full
-- configuration. For values shared between the CLI and Toml file, the CLI
-- takes priority.
--
-- For example, if both the CLI and Toml file specify the trash home, then
-- the CLI's value will be used.
--
-- @since 0.1
getConfiguration :: MonadIO m => m (Env, Command)
getConfiguration = do
  -- get CLI args
  args <- liftIO getArgs

  -- get toml config
  tomlConfig <- case args ^. #tomlConfigPath of
    -- 1. explicit toml config path given: read
    Just tomlPath -> readConfig tomlPath
    -- no toml config path given...
    Nothing -> do
      xdgConfig <- Dir.getXdgDirectory XdgConfig "safe-rm"
      let defPath = xdgConfig </> "config.toml"
      exists <- Dir.doesFileExist defPath
      if exists
        then -- 2. config exists at default path: read
          readConfig defPath
        else -- 3. no config exists: return default (empty)
          pure mempty

  -- merge share CLI and toml values
  let mergedConfig = mergeConfigs args tomlConfig
  trashHome <- trashOrDefault $ mergedConfig ^. #trashHome

  let fileLogPath = trashHome <//> ".log"
      logContext =
        MkLogContext
          { namespace = ["runner"],
            scribes = mkScribes mergedConfig fileLogPath
          }
      env =
        MkEnv
          { trashHome,
            logContext,
            fileLogPath
          }
  pure (env, args ^. #command)
  where
    readConfig fp = do
      contents <-
        liftIO (BS.readFile fp) >>= \contents' -> do
          case TEnc.decodeUtf8' contents' of
            Right txt -> pure txt
            Left err -> throwIO err
      case TOML.decode contents of
        Right cfg -> pure cfg
        Left tomlErr -> throwIO $ MkExceptionI @TomlDecode tomlErr

mkScribes :: TomlConfig -> PathI TrashLog -> HashMap Text Scribe
mkScribes mergedConfig (MkPathI trashLog) = Map.fromList scribes
  where
    scribes =
      ("console", consoleScribe) : mfileScribe
    -- Default to no file scribe. Only provide one if the user asks for it.
    mfileScribe = maybe [] lvlToScribe (mergedConfig ^. #fileLogLevel)
    lvlToScribe = (: []) . ("file",) . mkFileScribe

    -- Log to file. Full decoration.
    mkFileScribe fileLevel =
      MkScribe
        { logger = \ns mloc lvl msg -> do
            let fmt =
                  MkLogFormat
                    { logLoc = LogLocPartial <$> mloc,
                      namespace = Just ns,
                      withTimestamp = True,
                      newline = True
                    }
            exists <- Dir.doesFileExist trashLog
            formatted <- Format.formatLog fmt lvl msg
            -- TODO: just create the file in our setup so we are not doing this
            -- check every time.
            if exists
              then BS.appendFile trashLog (TEnc.encodeUtf8 formatted)
              else BS.writeFile trashLog (TEnc.encodeUtf8 formatted),
          logLevel = fileLevel
        }
    -- Log to console. Limited decoration.
    consoleScribe =
      MkScribe
        { logger = \_ _ lvl msg -> do
            let fmt =
                  MkLogFormat
                    { logLoc = Nothing,
                      namespace = Nothing,
                      withTimestamp = False,
                      newline = False
                    }
            Format.formatLog fmt lvl msg >>= putTextLn,
          logLevel = fromMaybe Error (mergedConfig ^. #consoleLogLevel)
        }

printIndex ::
  ( HasTrashHome env,
    Logger m,
    MonadReader env m,
    MonadIO m,
    Terminal m
  ) =>
  m ()
printIndex = SafeRm.getIndex >>= prettyDel

printMetadata ::
  ( HasTrashHome env,
    Logger m,
    MonadReader env m,
    MonadIO m,
    Terminal m
  ) =>
  m ()
printMetadata = SafeRm.getMetadata >>= prettyDel

prettyDel :: (Pretty a, Terminal m) => a -> m ()
prettyDel =
  putTextLn
    . renderStrict
    . layoutCompact
    . pretty

listToSet :: Hashable a => NonEmpty a -> HashSet a
listToSet = Set.fromList . NE.toList

-- | If the argument is given, returns it. Otherwise searches for the default
-- trash location.
--
-- @since 0.1
trashOrDefault :: MonadIO m => Maybe (PathI TrashHome) -> m (PathI TrashHome)
trashOrDefault = maybe getTrashHome pure

-- | Retrieves the default trash directory.
--
-- @since 0.1
getTrashHome :: MonadIO m => m (PathI TrashHome)
getTrashHome = MkPathI . (</> ".trash") <$> Dir.getHomeDirectory
