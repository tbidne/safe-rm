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
import SafeRm.Effects.Logger (LoggerContext)
import SafeRm.Effects.Logger qualified as Logger
import SafeRm.Effects.Terminal (Terminal)
import SafeRm.Prelude
import SafeRm.Runner.Env (Env)

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
      MonadReader env,
      -- | @since 0.1
      MonadIO,
      -- | @since 0.1
      Terminal,
      -- | @since 0.1
      MonadUnliftIO
    )
    via (ReaderT env m)

instance MonadIO m => MonadLogger (SafeRmT Env m) where
  monadLoggerLog loc _src lvl msg = do
    mhandle <- asks (preview (#logEnv % #logFile %? #handle))
    case mhandle of
      Nothing -> pure ()
      Just h -> do
        formatted <- Logger.formatLog True loc lvl msg
        let bs = Logger.logStrToBs formatted
        liftIO $ BS.hPut h bs

instance MonadIO m => LoggerContext (SafeRmT Env m) where
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
