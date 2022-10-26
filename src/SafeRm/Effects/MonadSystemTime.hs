{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'MonadSystemTime' class.
--
-- @since 0.1
module SafeRm.Effects.MonadSystemTime
  ( MonadSystemTime (..),
    Timestamp (..),
    toString,
    toText,
    fromText,
  )
where

import Control.Monad.Trans (MonadTrans (lift))
import Data.ByteString.Char8 qualified as Char8
import Data.Csv (FromField (parseField), ToField (toField))
import Data.Text qualified as T
import Data.Time.Format qualified as Format
import Data.Time.LocalTime qualified as Local
import Data.Time.LocalTime.Compat (LocalTime)
import SafeRm.Prelude

-- NOTE: We currently do not include any timezone information. We started
-- out doing so at first but then realized timezone parsing is unsatisfactory.
-- Parsing requires a locale, and the only one provided by the time package
-- (Format.defaultTimeLocale) is restricted to American timezones. The
-- time-conv package ostensibly fixes this, however it does not handle
-- daylight savings time i.e. parsing will fail on e.g. EDT, NZDT. Ideally we
-- would fix this upstream, though in the meantime we leave out timezone info
-- altogether.

-- | Represents a point in time.
--
-- @since 0.1
newtype Timestamp = MkTimestamp
  { unTimestamp :: LocalTime
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )
  deriving
    ( -- | @since 0.1
      Hashable
    )
    via LocalTime

makeFieldLabelsNoPrefix ''Timestamp

-- | @since 0.1
instance Pretty Timestamp where
  pretty = fromString . toString

-- | @since 0.1
instance FromField Timestamp where
  parseField =
    fmap MkTimestamp
      . Format.parseTimeM
        True
        -- NOTE: Change if we ever include timezone info.
        Format.defaultTimeLocale
        format
      . bs2Str

-- | @since 0.1
instance ToField Timestamp where
  toField = str2Bs . toString

-- | Class for retrieving the current system time.
--
-- @since 0.1
class Monad m => MonadSystemTime m where
  -- | @since 0.1
  getSystemTime :: HasCallStack => m Timestamp

-- | @since 0.1
instance MonadSystemTime IO where
  getSystemTime =
    MkTimestamp . Local.zonedTimeToLocalTime
      <$> liftIO Local.getZonedTime

-- | @since 0.1
instance MonadSystemTime m => MonadSystemTime (ReaderT e m) where
  getSystemTime = lift getSystemTime

-- | Formats the time.
--
-- @since 0.1
toString :: Timestamp -> String
toString =
  -- NOTE: If we ever handle timezones we will want to change this locale
  -- to one that handles more timezones (e.g see time-conv)
  Format.formatTime Format.defaultTimeLocale format
    . view #unTimestamp

-- | Formats the time.
--
-- @since 0.1
toText :: Timestamp -> Text
toText = T.pack . toString

-- | @since 0.1
fromText :: MonadFail f => Text -> f Timestamp
fromText =
  fmap MkTimestamp
    . Format.parseTimeM
      True
      -- NOTE: Change if we ever include timezone info.
      Format.defaultTimeLocale
      format
    . T.unpack

format :: String
format = "%Y-%m-%d %H:%M:%S"

bs2Str :: ByteString -> String
bs2Str = Char8.unpack . Char8.strip

str2Bs :: String -> ByteString
str2Bs = Char8.strip . Char8.pack
