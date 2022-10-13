{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides logging utilities.
--
-- @since 0.1
module SafeRm.Effects.Logger
  ( LoggerContext (..),
    Namespace (..),
    addNamespace,

    -- * Reading
    readLogLevel,
    logLevelStrings,

    -- * Formatting
    formatLog,

    -- * LogStr
    logStrToBs,
    logStrToText,
  )
where

import Control.Monad.Logger (LogStr, ToLogStr (toLogStr))
import Control.Monad.Logger.CallStack (LogLevel (LevelOther))
import Data.ByteString.Builder (Builder)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Text.Encoding.Error qualified as TEncError
import Language.Haskell.TH (Loc (loc_filename, loc_start))
import SafeRm.Data.Timestamp qualified as TS
import SafeRm.Prelude
import System.Log.FastLogger qualified as FL

-- | Logging namespace
--
-- @since 0.1
newtype Namespace = MkNamespace
  { -- | @since 0.1
    unNamespace :: Seq Text
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )
  deriving
    ( -- | @since 0.1
      Empty
    )
    via (Seq Text)

-- | @since 0.1
instance IsString Namespace where
  fromString = MkNamespace . Seq.singleton . T.pack

makeFieldLabelsNoPrefix ''Namespace

displayNamespace :: Namespace -> Text
displayNamespace =
  foldMap' id
    . Seq.intersperse "."
    . view #unNamespace

-- | Adds context to 'MonadLogger'.
--
-- @since 0.1
class MonadLogger m => LoggerContext m where
  -- | Retrieves the namespace.
  --
  -- @since 0.1
  getNamespace :: m Namespace

  -- | Locally modifies the namespace.
  --
  -- @since 0.1
  localNamespace :: (Namespace -> Namespace) -> m a -> m a

-- | Adds to the namespace.
--
-- @since 0.1
addNamespace :: LoggerContext m => Text -> m a -> m a
addNamespace txt = localNamespace (over' #unNamespace (|> txt))

-- | Reads the 'LogLevel'.
--
-- @since 0.1
readLogLevel :: MonadFail m => Text -> m (Maybe LogLevel)
readLogLevel "none" = pure Nothing
readLogLevel "error" = pure $ Just LevelError
readLogLevel "warn" = pure $ Just LevelWarn
readLogLevel "info" = pure $ Just LevelInfo
readLogLevel "debug" = pure $ Just LevelDebug
readLogLevel other =
  fail $
    mconcat
      [ "Expected log-level ",
        logLevelStrings,
        ", received: ",
        T.unpack other
      ]

-- | String description of possible log levels parsed by 'readLogLevel'.
--
-- @since 0.1
logLevelStrings :: String
logLevelStrings = "[none|error|warn|info|debug]"

-- | Produces a formatted 'LogStr'.
--
-- @since 0.1
formatLog ::
  (LoggerContext m, MonadIO m) =>
  ToLogStr msg =>
  Bool ->
  Loc ->
  LogLevel ->
  msg ->
  m LogStr
formatLog withNewline loc lvl msg = do
  -- TODO: abstract this
  timestampTxt <- toLogStr . TS.toString <$> liftIO TS.getCurrentLocalTime
  namespace <- getNamespace
  let locTxt = toLogStr $ partialLoc loc
      namespaceTxt = toLogStr $ displayNamespace namespace
      lvlTxt = toLogStr $ showLevel lvl
      msgTxt = toLogStr msg
      newline
        | withNewline = "\n"
        | otherwise = ""
      formatted =
        mconcat
          [ brackets timestampTxt,
            brackets namespaceTxt,
            brackets lvlTxt,
            brackets locTxt,
            " ",
            msgTxt,
            newline
          ]
  pure formatted

partialLoc :: Loc -> Builder
partialLoc loc =
  mconcat
    [ fromString $ view #loc_filename loc,
      ":" <> mkLine loc,
      ":" <> mkChar loc
    ]
  where
    mkLine = fromString . show . view (#loc_start % _1)
    mkChar = fromString . show . view (#loc_start % _2)

showLevel :: LogLevel -> Text
showLevel LevelDebug = "Debug"
showLevel LevelInfo = "Info"
showLevel LevelWarn = "Warn"
showLevel LevelError = "Error"
showLevel (LevelOther txt) = "Other " <> txt

-- LogStr uses ByteString's Builder internally, so we might as well use it
-- for constants.
brackets :: LogStr -> LogStr
brackets m = cLogStr "[" <> m <> cLogStr "]"

cLogStr :: Builder -> LogStr
cLogStr = toLogStr @Builder

-- | @since 0.1
logStrToBs :: LogStr -> ByteString
logStrToBs = FL.fromLogStr

-- | @since 0.1
logStrToText :: LogStr -> Text
logStrToText = TEnc.decodeUtf8With TEncError.lenientDecode . FL.fromLogStr
