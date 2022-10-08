-- | Provides the 'Logger' typeclass.
--
-- @since 0.1
module SafeRm.Effects.Logger
  ( -- * Types

    -- ** Class
    Logger (..),

    -- ** Data
    LogContext (..),
    LogLevel (..),
    readLogLevel,

    -- ** Functions

    -- * Log Levels
    logError,
    logWarn,
    logInfo,
    logDebug,

    -- * High level
    logShow,
    logString,
    logText,
    withExLogging,
    logErrorException,
    logWarnException,

    -- * Low level
    formatLog,
  )
where

import Data.Sequence qualified as Seq
import Data.Text qualified as T
import SafeRm.Data.Timestamp qualified as Timestamp
import SafeRm.Prelude

-- | Contains context for logging.
--
-- @since 0.1
data LogContext = MkLogContext
  { -- | List of namespaces.
    --
    -- @since 0.1
    namespace :: !(Seq Text),
    -- | The console level in which to log.
    --
    -- @since 0.1
    consoleLogLevel :: !LogLevel,
    -- | The file level in which to log.
    --
    -- @since 0.1
    fileLogLevel :: !LogLevel
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )

-- | Represents a terminal.
--
-- @since 0.1
type Logger :: (Type -> Type) -> Constraint
class Monad m => Logger m where
  -- | Logs a string.
  --
  -- @since 0.1
  putLog :: LogLevel -> Text -> m ()

  -- | Determines the level in which to log.
  --
  -- @since 0.1
  getContext :: m LogContext

  -- | Adds checkpoints.
  --
  -- @since 0.1
  addNamespace :: Text -> m a -> m a

-- | Log levels.
--
-- @since 0.1
data LogLevel
  = -- | No logging.
    --
    -- @since 0.1
    None
  | -- | Log error messages.
    --
    -- @since 0.1
    Error
  | -- | Log warn messages.
    --
    -- @since 0.1
    Warn
  | -- | Log info messages.
    --
    -- @since 0.1
    Info
  | -- | Log debug messages.
    --
    -- @since 0.1
    Debug
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )

-- | Formats a log.
--
-- @since 0.1
formatLog :: MonadIO m => Seq Text -> LogLevel -> Text -> m Text
formatLog namespace lvl msg = do
  t <- Timestamp.toText <$> Timestamp.getCurrentLocalTime
  let namespaces = foldMap' id $ Seq.intersperse "." namespace
      formatted =
        mconcat
          [ withBrackets False t,
            withBrackets False namespaces,
            withBrackets True (showt lvl),
            msg
          ]
  pure formatted
  where
    withBrackets False s = "[" <> s <> "]"
    withBrackets True s = "[" <> s <> "] "

-- | Logs at the 'Error'.
--
-- @since 0.1
logError :: Logger m => Text -> m ()
logError = logText Error

-- | Logs at the 'warn' level.
--
-- @since 0.1
logWarn :: Logger m => Text -> m ()
logWarn = logText Warn

-- | Logs at the 'Info' level.
--
-- @since 0.1
logInfo :: Logger m => Text -> m ()
logInfo = logText Info

-- | Logs at the 'Debug' level.
--
-- @since 0.1
logDebug :: Logger m => Text -> m ()
logDebug = logText Debug

-- | Runs the action, logging any exceptions before rethrowing.
--
-- @since 0.1
withExLogging :: (Logger m, MonadUnliftIO m) => m a -> m a
withExLogging = handleAny $ \e -> logWarnException e *> throwIO e

-- | Logs via show with current time and guarding.
--
-- @since 0.1
logShow :: (Logger m, Show a) => LogLevel -> a -> m ()
logShow lvl = logText lvl . showt

-- | 'String' version of 'logText'.
--
-- @since 0.1
logString :: Logger m => LogLevel -> String -> m ()
logString lvl = logText lvl . T.pack

-- | Alias for 'putLog'.
--
-- @since 0.1
logText :: Logger m => LogLevel -> Text -> m ()
logText = putLog

-- | Logs an 'Exception'. Uses 'logString' at the 'Error' level.
--
-- @since 0.1
logErrorException :: (Exception e, Logger m) => e -> m ()
logErrorException = logString Error . displayException

-- | Logs an 'Exception'. Uses 'logString' at the 'Warn' level.
--
-- @since 0.1
logWarnException :: (Exception e, Logger m) => e -> m ()
logWarnException = logString Warn . displayException

-- | Parses a log level.
--
-- @since 0.1
readLogLevel :: MonadFail m => Text -> m LogLevel
readLogLevel "none" = pure None
readLogLevel "error" = pure Error
readLogLevel "warn" = pure Warn
readLogLevel "info" = pure Info
readLogLevel "debug" = pure Debug
readLogLevel other =
  fail $
    mconcat
      [ "Expected log-level [none|error|warn|info|debug], received: ",
        T.unpack other
      ]
