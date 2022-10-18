{-# LANGUAGE TemplateHaskell #-}

-- | This modules provides an executable for running safe-rm.
--
-- @since 0.1
module SafeRm.Runner
  ( -- * Main functions
    runSafeRm,
    runCmd,

    -- * Helpers
    getEnv,
    getConfiguration,
  )
where

import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import SafeRm qualified
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex (TrashHome),
  )
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Effects.MonadCallStack (MonadCallStack, throwCS)
import SafeRm.Effects.MonadFsReader (MonadFsReader (readFile))
import SafeRm.Effects.MonadFsWriter (MonadFsWriter (createDirectoryIfMissing))
import SafeRm.Effects.MonadLoggerContext (MonadLoggerContext)
import SafeRm.Effects.MonadSystemTime (MonadSystemTime)
import SafeRm.Effects.MonadTerminal (MonadTerminal, putTextLn)
import SafeRm.Env (HasTrashHome)
import SafeRm.Exceptions
  ( ExceptionI (MkExceptionI),
    ExceptionIndex (TomlDecode),
    wrapCS,
  )
import SafeRm.Prelude
import SafeRm.Runner.Args
  ( TomlConfigPath
      ( TomlDefault,
        TomlNone,
        TomlPath
      ),
    getArgs,
  )
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
import SafeRm.Runner.Env
  ( Env (MkEnv, trashHome),
    LogEnv (MkLogEnv),
    LogFile (MkLogFile),
    finalizer,
    handle,
    logEnv,
    logFile,
    logLevel,
    logNamespace,
  )
import SafeRm.Runner.SafeRmT (usingSafeRmT)
import SafeRm.Runner.Toml (TomlConfig, mergeConfigs)
import System.Exit (ExitCode (ExitSuccess))
import System.IO qualified as IO
import TOML qualified
import UnliftIO.Directory (XdgDirectory (XdgConfig))
import UnliftIO.Directory qualified as Dir

-- | Entry point for running SafeRm. Does everything: reads CLI args,
-- optional Toml config, and creates the environment before running
-- SafeRm.
--
-- @since 0.1
runSafeRm ::
  ( MonadFsReader m,
    MonadFsWriter m,
    HasCallStack,
    MonadCallStack m,
    MonadUnliftIO m,
    MonadTerminal m,
    MonadSystemTime m
  ) =>
  m ()
runSafeRm =
  bracket
    getEnv
    closeLogging
    ( \(env, cmd) ->
        usingSafeRmT env (runCmd cmd)
    )
    -- NOTE: the doNothingOnSuccess has to be _outside_ the bracket to
    -- successfully catch the ExitSuccess.
    `catch` doNothingOnSuccess
    `catchAny` handleEx
  where
    closeLogging (e, _) = do
      let mFinalizer = e ^? #logEnv % #logFile %? #finalizer
      liftIO $ wrapCS $ fromMaybe (pure ()) mFinalizer

    doNothingOnSuccess ExitSuccess = pure ()
    doNothingOnSuccess ex = throwIO ex

    -- NOTE: Finally, print any exceptions to the console before exiting with
    -- failure. We need this _outside_ of the setup in case the setup itself
    -- fails.
    handleEx ex = liftIO $ do
      putTextLn $ T.pack $ displayException ex
      exitFailure

-- | Runs SafeRm in the given environment. This is useful in conjunction with
-- 'getConfiguration' as an alternative 'runSafeRm', when we want to use a
-- custom env.
runCmd ::
  ( MonadFsReader m,
    MonadFsWriter m,
    HasCallStack,
    HasTrashHome env,
    MonadLoggerContext m,
    MonadCallStack m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadTerminal m,
    MonadSystemTime m
  ) =>
  Command ->
  m ()
runCmd cmd = runCmd' cmd `catchAny` logEx
  where
    runCmd' = \case
      Delete paths -> SafeRm.delete paths
      DeletePerm force paths -> SafeRm.deletePermanently force paths
      Empty force -> SafeRm.emptyTrash force
      Restore paths -> SafeRm.restore paths
      List -> printIndex *> printMetadata
      Metadata -> printMetadata

    logEx ex = do
      $(logError) (displayExceptiont ex)
      throwIO ex

-- | Parses CLI 'Args' and optional 'TomlConfig' to produce the final Env used
-- by SafeRm.
--
-- @since 0.1
getEnv ::
  ( HasCallStack,
    MonadFsWriter m,
    MonadCallStack m,
    MonadUnliftIO m
  ) =>
  m (Env, Command)
getEnv = do
  (mergedConfig, command) <- getConfiguration

  trashHome <- trashOrDefault $ mergedConfig ^. #trashHome
  -- NOTE: Needed so below openFile command does not fail
  Paths.applyPathI (createDirectoryIfMissing False) trashHome

  logFile <- case join (mergedConfig ^. #logLevel) of
    Nothing -> pure Nothing
    Just lvl -> do
      let logPath = trashHome ^. #unPathI </> ".log"
      h <- liftIO $ wrapCS $ IO.openFile logPath IO.AppendMode
      pure $
        Just $
          MkLogFile
            { handle = h,
              logLevel = lvl,
              finalizer = IO.hFlush h `finally` IO.hClose h
            }
  let env =
        MkEnv
          { trashHome,
            logEnv =
              MkLogEnv
                { logFile,
                  logNamespace = "runner"
                }
          }
  pure (env, command)

-- | Parses CLI 'Args' and optional 'TomlConfig' to produce the user
-- configuration. For values shared between the CLI and Toml file, the CLI
-- takes priority.
--
-- For example, if both the CLI and Toml file specify the trash home, then
-- the CLI's value will be used.
--
-- @since 0.1
getConfiguration ::
  ( HasCallStack,
    MonadCallStack m,
    MonadUnliftIO m
  ) =>
  m (TomlConfig, Command)
getConfiguration = do
  -- get CLI args
  args <- liftIO getArgs

  -- get toml config
  tomlConfig <- case args ^. #tomlConfigPath of
    -- 1. explicit toml config path given: read
    TomlPath tomlPath -> readConfig tomlPath
    -- no toml config path given...
    TomlDefault -> do
      xdgConfig <- wrapCS $ Dir.getXdgDirectory XdgConfig "safe-rm"
      let defPath = xdgConfig </> "config.toml"
      exists <- wrapCS $ Dir.doesFileExist defPath
      if exists
        then -- 2. config exists at default path: read
          readConfig defPath
        else -- 3. no config exists: return default (empty)
          pure mempty
    -- 4. toml explicitly disabled
    TomlNone -> pure mempty

  -- merge share CLI and toml values
  let mergedConfig = mergeConfigs args tomlConfig
  pure (mergedConfig, args ^. #command)
  where
    readConfig fp = do
      contents <-
        liftIO (readFile fp) >>= \contents' -> do
          case TEnc.decodeUtf8' contents' of
            Right txt -> pure txt
            Left err -> throwIO err
      case TOML.decode contents of
        Right cfg -> pure cfg
        Left tomlErr -> throwCS $ MkExceptionI @TomlDecode tomlErr

printIndex ::
  ( MonadFsReader m,
    HasCallStack,
    HasTrashHome env,
    MonadLoggerContext m,
    MonadCallStack m,
    MonadIO m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  m ()
printIndex = SafeRm.getIndex >>= prettyDel

printMetadata ::
  ( MonadFsReader m,
    HasCallStack,
    HasTrashHome env,
    MonadLoggerContext m,
    MonadCallStack m,
    MonadIO m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  m ()
printMetadata = SafeRm.getMetadata >>= prettyDel

prettyDel :: (HasCallStack, Pretty a, MonadTerminal m) => a -> m ()
prettyDel =
  putTextLn
    . renderStrict
    . layoutCompact
    . pretty

-- | If the argument is given, returns it. Otherwise searches for the default
-- trash location.
--
-- @since 0.1
trashOrDefault ::
  ( HasCallStack,
    MonadCallStack m,
    MonadUnliftIO m
  ) =>
  Maybe (PathI TrashHome) ->
  m (PathI TrashHome)
trashOrDefault = maybe getTrashHome pure

-- | Retrieves the default trash directory.
--
-- @since 0.1
getTrashHome ::
  ( HasCallStack,
    MonadCallStack m,
    MonadUnliftIO m
  ) =>
  m (PathI TrashHome)
getTrashHome = MkPathI . (</> ".trash") <$> wrapCS Dir.getHomeDirectory
