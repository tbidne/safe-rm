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

import Data.ByteString qualified as BS
import Data.Sequence (Seq (Empty, (:<|)))
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
import SafeRm.Effects.MonadCallStack (MonadCallStack, throwCallStack)
import SafeRm.Effects.MonadFsReader (MonadFsReader (..))
import SafeRm.Effects.MonadFsWriter (MonadFsWriter (hPut))
import SafeRm.Effects.MonadLoggerContext (MonadLoggerContext)
import SafeRm.Effects.MonadLoggerContext qualified as Logger
import SafeRm.Effects.MonadSystemTime (MonadSystemTime)
import SafeRm.Effects.MonadTerminal (MonadTerminal (putStrLn))
import SafeRm.Exception (PathNotFoundE (MkPathNotFoundE), withStackTracing)
import SafeRm.Prelude
import SafeRm.Runner.Env (Env, LogFile, handle, logLevel)
import UnliftIO.Directory qualified as Dir

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
      -- | @since 0.1
      MonadFsWriter,
      -- | @since 0.1
      Monad,
      -- | @since 0.1
      MonadCallStack,
      -- | @since 0.1
      MonadReader env,
      -- | @since 0.1
      MonadIO,
      -- | @since 0.1
      MonadTerminal,
      -- | @since 0.1
      MonadSystemTime,
      -- | @since 0.1
      MonadUnliftIO
    )
    via (ReaderT env m)

-- | @since 0.1
instance
  (MonadFsWriter m, MonadSystemTime m) =>
  MonadLogger (SafeRmT Env m)
  where
  monadLoggerLog loc _src lvl msg = do
    mhandle <- asks (preview (#logEnv % #logFile %? handleAndLevel))
    case mhandle of
      Nothing -> pure ()
      Just (handle, logLevel) ->
        when (logLevel <= lvl) $ do
          formatted <- Logger.formatLog True loc lvl msg
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
  (MonadFsWriter m, MonadSystemTime m) =>
  MonadLoggerContext (SafeRmT Env m)
  where
  getNamespace = asks (view (#logEnv % #logNamespace))
  localNamespace = local . over' (#logEnv % #logNamespace)

-- | @since 0.1
instance
  ( MonadCallStack m,
    MonadFsWriter m,
    MonadSystemTime m,
    MonadTerminal m,
    MonadUnliftIO m
  ) =>
  MonadFsReader (SafeRmT Env m)
  where
  getFileSize path = withStackTracing $ do
    liftIO (findLargestPaths cfg path) >>= \case
      (Empty, MkSubPathData (x :<| _)) -> pure $ MkBytes $ x ^. #size
      (errs@(_ :<| _), MkSubPathData (x :<| _)) -> do
        -- We received a value but had some errors.
        putStrLn "Encountered errors retrieving size. See logs."
        for_ errs $ \e -> $(logError) (displayExceptiont e)
        pure $ MkBytes $ x ^. #size
      -- Didn't receive a value; must have encountered errors
      (errs, MkSubPathData Empty) -> do
        putStrLn "Could not retrieve size, defaulting to 0. See logs."
        for_ errs $ \e -> $(logError) (displayExceptiont e)
        pure $ MkBytes 0
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

  readFile f = do
    exists <- doesFileExist f
    unless exists $
      throwCallStack $
        MkPathNotFoundE f
    withStackTracing (liftIO $ BS.readFile f)

  doesFileExist = withStackTracing . Dir.doesFileExist
  doesDirectoryExist = withStackTracing . Dir.doesDirectoryExist
  doesPathExist = withStackTracing . Dir.doesPathExist
  canonicalizePath = withStackTracing . Dir.canonicalizePath
  listDirectory = withStackTracing . Dir.listDirectory

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
