{-# LANGUAGE TemplateHaskell #-}

-- | Provides the 'SafeRmT' type for running SafeRm.
--
-- @since 0.1
module SafeRm.Runner.SafeRmT
  ( SafeRmT (MkSafeRmT),
    runSafeRmT,
    usingSafeRmT,
  )
where

import Data.Sequence (Seq (Empty, (:<|)))
import Effects.MonadFs (MonadFsReader (..), MonadFsWriter (hPut))
import Effects.MonadLoggerNamespace (MonadLoggerNamespace, defaultLogFormatter)
import Effects.MonadLoggerNamespace qualified as Logger
import Effects.MonadTime (MonadTime)
import PathSize
  ( Config
      ( MkConfig,
        exclude,
        filesOnly,
        maxDepth,
        numPaths,
        searchAll,
        strategy
      ),
    SubPathData (MkSubPathData),
    findLargestPaths,
  )
import SafeRm.Prelude
import SafeRm.Runner.Env (Env, LogFile, handle, logLevel)

-- | `SafeRmT` is the main application type that runs shell commands.
--
-- @since 0.1
type SafeRmT :: Type -> (Type -> Type) -> Type -> Type
newtype SafeRmT env m a = MkSafeRmT (ReaderT env m a)
  deriving
    ( -- | @since 0.1
      Functor,
      -- | @since 0.1
      Applicative,
      -- | @since 0.1
      Monad,
      -- | @since 0.1
      MonadCallStack,
      -- | @since 0.1
      MonadFsWriter,
      -- | @since 0.1
      MonadReader env,
      -- | @since 0.1
      MonadIO,
      -- | @since 0.1
      MonadTerminal,
      -- | @since 0.1
      MonadTime,
      -- | @since 0.1
      MonadUnliftIO
    )
    via (ReaderT env m)

-- | @since 0.1
instance
  (MonadCallStack m, MonadFsWriter m, MonadTime m) =>
  MonadLogger (SafeRmT Env m)
  where
  monadLoggerLog loc _src lvl msg = do
    mhandle <- asks (preview (#logEnv % #logFile %? handleAndLevel))
    case mhandle of
      Nothing -> pure ()
      Just (handle, logLevel) ->
        when (logLevel <= lvl) $ do
          formatted <- Logger.formatLog (defaultLogFormatter loc) lvl msg
          let bs = Logger.logStrToBs formatted
          hPut handle bs
    where
      handleAndLevel :: Lens' LogFile (Handle, LogLevel)
      handleAndLevel =
        lens
          (\lf -> bimap (view #handle) (view #logLevel) (lf, lf))
          (\lf (h, ll) -> lf {logLevel = ll, handle = h})

-- | @since 0.1
instance
  (MonadCallStack m, MonadFsWriter m, MonadTime m) =>
  MonadLoggerNamespace (SafeRmT Env m)
  where
  getNamespace = asks (view (#logEnv % #logNamespace))
  localNamespace = local . over' (#logEnv % #logNamespace)

-- | @since 0.1
instance
  ( MonadCallStack m,
    MonadFsWriter m,
    MonadIO m,
    MonadTime m,
    MonadTerminal m
  ) =>
  MonadFsReader (SafeRmT Env m)
  where
  getFileSize path = checkpointCallStack $ do
    liftIO (findLargestPaths cfg path) >>= \case
      (Empty, MkSubPathData (x :<| _)) -> pure $ x ^. #size
      (errs@(_ :<| _), MkSubPathData (x :<| _)) -> do
        -- We received a value but had some errors.
        putStrLn "Encountered errors retrieving size. See logs."
        for_ errs $ \e -> $(logError) (displayExceptiont e)
        pure $ x ^. #size
      -- Didn't receive a value; must have encountered errors
      (errs, MkSubPathData Empty) -> do
        putStrLn "Could not retrieve size, defaulting to 0. See logs."
        for_ errs $ \e -> $(logError) (displayExceptiont e)
        pure 0
    where
      cfg =
        MkConfig
          { searchAll = True,
            maxDepth = Just 0,
            exclude = mempty,
            filesOnly = False,
            numPaths = Just 1,
            strategy = mempty
          }

  -- reuse IO's impl
  readFile = liftIO . readFile
  getHomeDirectory = liftIO getHomeDirectory
  getXdgConfig = liftIO . getXdgConfig
  doesFileExist = liftIO . doesFileExist
  doesDirectoryExist = liftIO . doesDirectoryExist
  doesPathExist = liftIO . doesPathExist
  canonicalizePath = liftIO . canonicalizePath
  listDirectory = liftIO . listDirectory

-- | Runs a 'SafeRmT' with the given @env@.
--
-- @since 0.1
runSafeRmT :: SafeRmT env m a -> env -> m a
runSafeRmT (MkSafeRmT rdr) = runReaderT rdr

-- | Flipped 'runSafeRmT'
--
-- @since 0.1
usingSafeRmT :: env -> SafeRmT env m a -> m a
usingSafeRmT = flip runSafeRmT
