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
import SafeRm.Effects.MonadCallStack (MonadCallStack)
import SafeRm.Effects.MonadFsReader (MonadFsReader)
import SafeRm.Effects.MonadFsWriter (MonadFsWriter)
import SafeRm.Effects.MonadLoggerContext (MonadLoggerContext)
import SafeRm.Effects.MonadLoggerContext qualified as Logger
import SafeRm.Effects.MonadSystemTime (MonadSystemTime)
import SafeRm.Effects.MonadTerminal (MonadTerminal)
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
      MonadFsReader,
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
instance (MonadIO m, MonadSystemTime m) => MonadLogger (SafeRmT Env m) where
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
instance (MonadIO m, MonadSystemTime m) => MonadLoggerContext (SafeRmT Env m) where
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
