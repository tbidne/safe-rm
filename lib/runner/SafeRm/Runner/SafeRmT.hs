-- | Provides the 'SafeRmT' type for running SafeRm.
--
-- @since 0.1
module SafeRm.Runner.SafeRmT
  ( SafeRmT,
    runSafeRmT,
    usingSafeRmT,
    -- putLogFile,
  )
where

import SafeRm.Effects.Logger
  ( LogContext (namespace),
    Logger (addNamespace, getContext),
  )
import SafeRm.Effects.Logger qualified as Logger
import SafeRm.Effects.Terminal (Terminal)
import SafeRm.Env (Env (logContext))
import SafeRm.Prelude

-- import System.Console.Pretty (Color)
-- import System.Console.Pretty qualified as P

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
instance Monad m => Logger (SafeRmT Env m) where
  getContext = asks (view #logContext)

  localContext = local . over' #logContext

  addNamespace t = local (over' (#logContext % #namespace) appendNS)
    where
      appendNS = (|> t)

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
