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
    logInfo,
    logDebug,

    -- * High level
    logShow,
    logString,
    logText,
    withExLogging,
    logException,

    -- * Low level
    guardWithTime,
    logWithTime,
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
    -- | The level in which to log.
    --
    -- @since 0.1
    logLevel :: !LogLevel
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
  putLog :: Text -> m ()

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

-- | If the parameter 'LogLevel' is > than 'logLevel', calls
-- 'logWithTime'. Otherwise does nothing.
--
-- @since 0.1
guardWithTime :: (Logger m, MonadIO m) => LogLevel -> Text -> m ()
guardWithTime lvl msg = do
  baseLevel <- view #logLevel <$> getContext
  when (lvl <= baseLevel) (logWithTime lvl msg)

-- | Logs a string, prepending the current time.
--
-- @since 0.1
logWithTime :: (Logger m, MonadIO m) => LogLevel -> Text -> m ()
logWithTime lvl str = do
  t <- Timestamp.getCurrentLocalTime
  namespace <- view #namespace <$> getContext
  let namespaces = foldMap' id $ Seq.intersperse "." namespace
      formatted =
        mconcat
          [ withBrackets False (Timestamp.toText t),
            withBrackets False namespaces,
            withBrackets True (showt lvl),
            str
          ]
  putLog formatted
  where
    withBrackets False s = "[" <> s <> "]"
    withBrackets True s = "[" <> s <> "] "

-- | Logs at the 'Error' level with current time and guarding.
--
-- @since 0.1
logError :: (Logger m, MonadIO m) => Text -> m ()
logError = logText Error

-- | Logs at the 'Info' level with current time and guarding.
--
-- @since 0.1
logInfo :: (Logger m, MonadIO m) => Text -> m ()
logInfo = logText Info

-- | Logs at the 'Debug' level with current time and guarding.
--
-- @since 0.1
logDebug :: (Logger m, MonadIO m) => Text -> m ()
logDebug = logText Debug

-- | Runs the action, logging any exceptions before rethrowing.
--
-- @since 0.1
withExLogging :: (Logger m, MonadUnliftIO m) => m a -> m a
withExLogging = handleAny $ \e -> logException e *> throwIO e

-- | Logs via show with current time and guarding.
--
-- @since 0.1
logShow :: (Logger m, MonadIO m, Show a) => LogLevel -> a -> m ()
logShow lvl = logText lvl . showt

-- | 'String' version of 'logText'.
--
-- @since 0.1
logString :: (Logger m, MonadIO m) => LogLevel -> String -> m ()
logString lvl = logText lvl . T.pack

-- | Alias for 'guardWithTime'.
--
-- @since 0.1
logText :: (Logger m, MonadIO m) => LogLevel -> Text -> m ()
logText = guardWithTime

-- | Logs an 'Exception'. Uses 'logString' at the 'Error' level.
--
-- @since 0.1
logException :: (Exception e, Logger m, MonadIO m) => e -> m ()
logException = logString Error . displayException

-- | Parses a log level.
--
-- @since 0.1
readLogLevel :: MonadFail m => Text -> m LogLevel
readLogLevel "none" = pure None
readLogLevel "error" = pure Error
readLogLevel "info" = pure Info
readLogLevel "debug" = pure Debug
readLogLevel other =
  fail $
    mconcat
      [ "Expected log-level [none|error|info|debug], received: ",
        T.unpack other
      ]
