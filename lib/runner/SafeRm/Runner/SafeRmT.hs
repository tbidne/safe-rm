-- | Provides the 'SafeRmT' type for running SafeRm.
--
-- @since 0.1
module SafeRm.Runner.SafeRmT
  ( SafeRmT,
    runSafeRmT,
    usingSafeRmT,
  )
where

import SafeRm.Effects.Terminal (Terminal)
import SafeRm.Env (Env (..))
import SafeRm.Prelude

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

-- | @since 0.1
instance MonadIO m => Katip (SafeRmT Env m) where
  getLogEnv = asks (view #logEnv)

  localLogEnv f = local (over' #logEnv f)

-- | @since 0.1
instance MonadIO m => KatipContext (SafeRmT Env m) where
  getKatipContext = asks (view #logContexts)
  localKatipContext f = local (over' #logContexts f)
  getKatipNamespace = asks (view #logNamespace)
  localKatipNamespace f = local (over' #logNamespace f)

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
