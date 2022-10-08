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
  ( LogContext (consoleLogLevel, fileLogLevel, namespace),
    Logger (addNamespace, getContext, putLog),
  )
import SafeRm.Effects.Logger qualified as Logger
import SafeRm.Effects.Terminal (Terminal, putTextLn)
import SafeRm.Env (Env (fileLogPath, logContext))
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
instance (MonadIO m, Terminal m) => Logger (SafeRmT Env m) where
  putLog lvl msg = do
    consoleLogLevel <- asks (view (#logContext % #consoleLogLevel))
    fileLogLevel <- asks (view (#logContext % #fileLogLevel))
    MkPathI fileLogPath <- asks (view #fileLogPath)
    namespace <- view #namespace <$> getContext
    formatted <- Logger.formatLog namespace lvl msg

    when (lvl <= consoleLogLevel) (putTextLn formatted)
    when (lvl <= fileLogLevel) $ do
      exists <- Dir.doesFileExist fileLogPath
      let formatted' = formatted <> "\n"
      liftIO $
        if exists
          then BS.appendFile fileLogPath (TEnc.encodeUtf8 formatted')
          else BS.writeFile fileLogPath (TEnc.encodeUtf8 formatted')

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
