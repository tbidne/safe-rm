-- | Provides the 'SafeRmT' type for running SafeRm.
--
-- @since 0.1
module SafeRm.Runner.SafeRmT
  ( SafeRmT,
    runSafeRmT,
    usingSafeRmT,
  )
where

import Data.ByteString qualified as BS
import SafeRm.Effects.FileSystemReader (FileSystemReader)
import SafeRm.Effects.FileSystemWriter (FileSystemWriter)
import SafeRm.Effects.Logger (LoggerContext)
import SafeRm.Effects.Logger qualified as Logger
import SafeRm.Effects.MonadCallStack (MonadCallStack)
import SafeRm.Effects.Terminal (Terminal)
import SafeRm.Effects.Timing (Timing)
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
      FileSystemReader,
      -- | @since 0.1
      FileSystemWriter,
      -- | @since 0.1
      Monad,
      -- | @since 0.1
      MonadCallStack,
      -- | @since 0.1
      MonadReader env,
      -- | @since 0.1
      MonadIO,
      -- | @since 0.1
      Terminal,
      -- | @since 0.1
      Timing,
      -- | @since 0.1
      MonadUnliftIO
    )
    via (ReaderT env m)

-- | @since 0.1
instance (MonadIO m, Timing m) => MonadLogger (SafeRmT Env m) where
  monadLoggerLog loc _src lvl msg = do
    mhandle <- asks (preview (#logEnv % #logFile %? handleAndLevel))
    case mhandle of
      Nothing -> pure ()
      Just (handle, logLevel) ->
        when (logLevel <= lvl) $ do
          formatted <- Logger.formatLog True loc lvl msg
          let bs = Logger.logStrToBs formatted
          liftIO $ BS.hPut handle bs
    where
      handleAndLevel :: Lens' LogFile (Handle, LogLevel)
      handleAndLevel =
        lens
          (\lf -> bimap (view #handle) (view #logLevel) (lf, lf))
          (\lf (h, ll) -> lf {logLevel = ll, handle = h})

-- | @since 0.1
instance (MonadIO m, Timing m) => LoggerContext (SafeRmT Env m) where
  getNamespace = asks (view (#logEnv % #logNamespace))
  localNamespace = local . over' (#logEnv % #logNamespace)

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
