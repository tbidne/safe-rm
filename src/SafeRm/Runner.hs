{-# LANGUAGE TemplateHaskell #-}

-- | This modules provides an executable for running safe-rm.
--
-- @since 0.1
module SafeRm.Runner
  ( -- * Main functions
    runSafeRm,
    runCmd,

    -- * Helpers
    getConfiguration,
  )
where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import SafeRm qualified
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex (TrashHome),
  )
import SafeRm.Effects.Logger (LoggerContext)
import SafeRm.Effects.Terminal (Terminal, putTextLn)
import SafeRm.Env (HasTrashHome)
import SafeRm.Exceptions
  ( ExceptionI (MkExceptionI),
    ExceptionIndex (TomlDecode),
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
runSafeRm :: (MonadUnliftIO m, Terminal m) => m ()
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
      liftIO $ fromMaybe (pure ()) mFinalizer

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
  ( HasTrashHome env,
    LoggerContext m,
    MonadReader env m,
    MonadUnliftIO m,
    Terminal m
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
getEnv :: MonadIO m => m (Env, Command)
getEnv = do
  (mergedConfig, command) <- getConfiguration
  trashHome <- trashOrDefault $ mergedConfig ^. #trashHome

  logFile <- case join (mergedConfig ^. #logLevel) of
    Nothing -> pure Nothing
    Just lvl -> do
      let logPath = trashHome ^. #unPathI </> ".log"
      -- FIXME: if the trash directory does not exist yet then this will fail!
      h <- liftIO $ IO.openFile logPath IO.AppendMode
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
getConfiguration :: MonadIO m => m (TomlConfig, Command)
getConfiguration = do
  -- get CLI args
  args <- liftIO getArgs

  -- get toml config
  tomlConfig <- case args ^. #tomlConfigPath of
    -- 1. explicit toml config path given: read
    TomlPath tomlPath -> readConfig tomlPath
    -- no toml config path given...
    TomlDefault -> do
      xdgConfig <- Dir.getXdgDirectory XdgConfig "safe-rm"
      let defPath = xdgConfig </> "config.toml"
      exists <- Dir.doesFileExist defPath
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
        liftIO (BS.readFile fp) >>= \contents' -> do
          case TEnc.decodeUtf8' contents' of
            Right txt -> pure txt
            Left err -> throwIO err
      case TOML.decode contents of
        Right cfg -> pure cfg
        Left tomlErr -> throwIO $ MkExceptionI @TomlDecode tomlErr

printIndex ::
  ( HasTrashHome env,
    LoggerContext m,
    MonadIO m,
    MonadReader env m,
    Terminal m
  ) =>
  m ()
printIndex = SafeRm.getIndex >>= prettyDel

printMetadata ::
  ( HasTrashHome env,
    LoggerContext m,
    MonadIO m,
    MonadReader env m,
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
