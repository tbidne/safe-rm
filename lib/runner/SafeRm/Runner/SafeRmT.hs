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
import Data.Text.Encoding qualified as TEnc
import SafeRm.Data.Paths (PathI (MkPathI))
import SafeRm.Effects.Logger
  ( LogContext (namespace),
    Logger (addNamespace, getContext, putLog),
  )
import SafeRm.Effects.Terminal (Terminal)
import SafeRm.Env (Env (logContext, logPath))
import SafeRm.Prelude
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
instance MonadIO m => Logger (SafeRmT Env m) where
  putLog t = do
    MkPathI fp <- asks (view #logPath)
    exists <- Dir.doesFileExist fp
    liftIO $
      if exists
        then BS.appendFile fp (TEnc.encodeUtf8 t')
        else BS.writeFile fp (TEnc.encodeUtf8 t')
    where
      t' = t <> "\n"

  getContext = asks (view #logContext)

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
