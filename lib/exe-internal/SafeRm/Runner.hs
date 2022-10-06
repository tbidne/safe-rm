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
import SafeRm qualified
import SafeRm.Args
  ( Args (command, tomlConfigPath),
    SafeRmCommand
      ( SafeRmCommandDelete,
        SafeRmCommandEmpty,
        SafeRmCommandList,
        SafeRmCommandMetadata,
        SafeRmCommandPermDelete,
        SafeRmCommandRestore
      ),
    getArgs,
  )
import SafeRm.Data.Paths (PathI (MkPathI), PathIndex (TrashHome))
import SafeRm.Effects.Terminal (Terminal, putTextLn)
import SafeRm.Env (Env (MkEnv, trashHome, verbose), HasTrashHome)
import SafeRm.Exceptions
  ( ExceptionI (MkExceptionI),
    ExceptionIndex (TomlDecode),
  )
import SafeRm.Prelude
import SafeRm.Toml (TomlConfig (trashHome, verbose), mergeConfigs)
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

  case cmd of
    SafeRmCommandDelete paths ->
      runReaderT (SafeRm.delete (listToSet paths)) env
    SafeRmCommandPermDelete force paths ->
      runReaderT (SafeRm.deletePermanently force (listToSet paths)) env
    SafeRmCommandEmpty force -> runReaderT (SafeRm.empty force) env
    SafeRmCommandRestore paths ->
      runReaderT (SafeRm.restore (listToSet paths)) env
    SafeRmCommandList ->
      runReaderT (printIndex *> printMetadata) env
    SafeRmCommandMetadata ->
      runReaderT printMetadata env

-- | Retrieves the configuration and runs the param function.
--
-- @since 0.1
withEnv :: MonadIO m => ((Env, SafeRmCommand) -> m a) -> m a
withEnv = (>>=) getConfiguration

-- | Parses CLI 'Args' and optional 'TomlConfig' to produce the full
-- configuration. For values shared between the CLI and Toml file, the CLI
-- takes priority.
--
-- For example, if both the CLI and Toml file specify the trash home, then
-- the CLI's value will be used.
--
-- @since 0.1
getConfiguration :: MonadIO m => m (Env, SafeRmCommand)
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
  let env =
        MkEnv
          { trashHome,
            verbose = fromMaybe False (mergedConfig ^. #verbose)
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

printIndex ::
  ( HasTrashHome env,
    MonadReader env m,
    MonadIO m,
    Terminal m
  ) =>
  m ()
printIndex = SafeRm.getIndex >>= prettyDel

printMetadata ::
  ( HasTrashHome env,
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
