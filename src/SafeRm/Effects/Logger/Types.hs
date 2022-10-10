-- | Provides logging types.
--
-- @since 0.1
module SafeRm.Effects.Logger.Types
  ( Logger (..),
    LogContext (..),
    LogLevel (..),
    readLogLevel,
    logLevelStrings,
    Scribe (..),
  )
where

import Data.Text qualified as T
import Language.Haskell.TH.Syntax (Lift, Loc)
import SafeRm.Prelude

-- | A 'Scribe' is a wrapper for a function that does the actual logging
-- e.g. writing to a file or logging to the console.
--
-- @since 0.1
data Scribe = MkScribe
  { -- | The logging function.
    --
    -- @since 0.1
    logger :: Seq Text -> Maybe Loc -> LogLevel -> Text -> IO (),
    -- | The level at which this scribe logs.
    logLevel :: LogLevel
    -- TODO: colors here
  }
  deriving stock
    ( -- | @since 0.1
      Generic
    )

-- | Contains context for logging.
--
-- @since 0.1
data LogContext = MkLogContext
  { -- | Map of names to scribes.
    --
    -- @since 0.1
    scribes :: !(HashMap Text Scribe),
    -- | The namespace.
    --
    -- @since 0.1
    namespace :: !(Seq Text)
  }
  deriving stock
    ( -- | @since 0.1
      Generic
    )

-- | Represents a terminal.
--
-- @since 0.1
type Logger :: (Type -> Type) -> Constraint
class Monad m => Logger m where
  -- | Retrieves the logging context.
  --
  -- @since 0.1
  getContext :: m LogContext

  -- | Locally modifies the context.
  --
  -- @since 0.1
  localContext :: (LogContext -> LogContext) -> m a -> m a

  -- | Adds a namespace.
  --
  -- @since 0.1
  addNamespace :: Text -> m a -> m a

-- | Log levels. The 'Semigroup' instance takes the largest by 'Ord'
-- i.e. 'Debug'.
--
-- @since 0.1
data LogLevel
  = -- | Log error messages.
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
      Bounded,
      -- | @since 0.1
      Eq,
      -- | @since 0.1
      Lift,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Semigroup LogLevel where
  (<>) = min

-- | @since 0.1
instance Monoid LogLevel where
  mempty = minBound

-- | Parses a log level.
--
-- @since 0.1
readLogLevel :: MonadFail m => Text -> m LogLevel
readLogLevel "error" = pure Error
readLogLevel "warn" = pure Warn
readLogLevel "info" = pure Info
readLogLevel "debug" = pure Debug
readLogLevel other =
  fail $
    mconcat
      [ "Expected log-level ",
        logLevelStrings,
        ", received: ",
        T.unpack other
      ]

logLevelStrings :: String
logLevelStrings = "[error|warn|info|debug]"
