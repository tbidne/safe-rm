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
import Data.HashSet qualified as Set
import Data.List.NonEmpty qualified as NE
import Data.Text.Encoding qualified as TEnc
import Katip
  ( ColorStrategy (ColorIfTerminal, ColorLog),
    Verbosity (V0, V2),
  )
import Katip qualified as K
import SafeRm qualified
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex (TrashHome, TrashLog),
    (<//>),
  )
import SafeRm.Effects.Logger qualified as Logger
import SafeRm.Effects.Terminal (Terminal, putTextLn)
import SafeRm.Env
  ( Env (MkEnv, logContexts, logEnv, logNamespace, trashHome),
    HasTrashHome,
  )
import SafeRm.Exceptions
  ( ExceptionI (MkExceptionI),
    ExceptionIndex (TomlDecode),
  )
import SafeRm.Prelude
import SafeRm.Runner.Args (getArgs)
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
import SafeRm.Runner.Toml (TomlConfig, mergeConfigs)
import System.Exit (ExitCode (ExitSuccess))
import System.IO qualified as IO
import TOML qualified
import UnliftIO.Directory (XdgDirectory (XdgConfig))
import UnliftIO.Directory qualified as Dir

-- | Reads CLI args, optional Toml config, and runs SafeRm.
--
-- @since 0.1
runSafeRm :: (MonadUnliftIO m, Terminal m) => m ()
runSafeRm =
  bracket
    getEnv
    closeScribes
    ( \(env, cmd) ->
        usingSafeRmT env $
          runCmd cmd `catchAny` handleSafeRmEx
    )
    -- NOTE: the doNothing has to be _outside_ the bracket to successfully
    -- catch the ExitSuccess
    `catch` doNothingOnSuccess
  where
    closeScribes =
      liftIO
        . K.closeScribes
        . view (_1 % #logEnv)

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

    handleSafeRmEx ex = do
      -- REVIEW: is this good enough?
      $(K.logTM) ErrorS (K.ls $ displayException ex)
      throwIO ex

-- | Retrieves the configuration and runs the param function.
--
-- @since 0.1
withEnv :: MonadIO m => ((Env, Command) -> m a) -> m a
withEnv = (>>=) getEnv

-- | Parses CLI 'Args' and optional 'TomlConfig' to produce the final Env used
-- by SafeRm.
--
-- @since 0.1
getEnv :: MonadIO m => m (Env, Command)
getEnv = do
  (mergedConfig, command) <- getConfiguration
  trashHome <- trashOrDefault $ mergedConfig ^. #trashHome

  logEnv <- liftIO $ mkLogEnv mergedConfig (trashHome <//> ".log")
  let env =
        MkEnv
          { trashHome,
            logEnv,
            logContexts = mempty,
            logNamespace = "runner"
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

mkLogEnv :: TomlConfig -> PathI TrashLog -> IO LogEnv
mkLogEnv mergedConfig (MkPathI trashLog) = do
  initLogEnv <- K.initLogEnv "safe-rm" environment

  consoleEnv <- case mergedConfig ^. #consoleLog of
    Just Nothing -> pure initLogEnv
    other -> do
      let consoleSeverity = fromMaybe ErrorS (join other)
      consoleScribe <-
        K.mkHandleScribeWithFormatter
          Logger.consoleFormatter
          ColorIfTerminal
          IO.stdout
          (K.permitItem consoleSeverity)
          V0

      K.registerScribe
        "console"
        consoleScribe
        K.defaultScribeSettings
        initLogEnv

  case mergedConfig ^. #fileLog of
    Nothing -> pure consoleEnv
    other -> do
      let fileSeverity = fromMaybe ErrorS (join other)
      fileScribe <- mkFileScribe trashLog (K.permitItem fileSeverity) V2
      K.registerScribe "file" fileScribe K.defaultScribeSettings consoleEnv
  where
    environment = "production"

-- NOTE: this is copied from katip but with our custom formatter
mkFileScribe :: FilePath -> K.PermitFunc -> Verbosity -> IO Scribe
mkFileScribe f permitF verb = do
  -- FIXME: if the trash directory does not exist yet then this will fail!
  h <- IO.openFile f IO.AppendMode
  Scribe logger finalizer permit <-
    K.mkHandleScribeWithFormatter
      Logger.fileFormatter
      (ColorLog False)
      h
      permitF
      verb
  pure (Scribe logger (finalizer `finally` IO.hClose h) permit)

printIndex ::
  ( HasTrashHome env,
    KatipContext m,
    MonadReader env m,
    Terminal m
  ) =>
  m ()
printIndex = SafeRm.getIndex >>= prettyDel

printMetadata ::
  ( HasTrashHome env,
    KatipContext m,
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
