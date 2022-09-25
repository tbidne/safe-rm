-- | Timestamp functionality.
--
-- @since 0.1
module Del.Data.Timestamp
  ( Timestamp (..),
    getCurrentLocalTime,
  )
where

import Data.ByteString.Char8 qualified as Char8
import Data.Csv (FromField (parseField), ToField (toField))
import Data.Time.Format qualified as Format
import Data.Time.LocalTime (LocalTime)
import Data.Time.LocalTime qualified as Local
import Del.Prelude

-- NOTE: We currently do not include any timezone information. We started
-- out doing so at first but then realized timezone parsing is unsatisfactory.
-- Parsing requires a locale, and the only one provided by the time package
-- (Format.defaultTimeLocale) is restricted to American timezones. The
-- time-conv package ostensibly fixes this, however it does not handle
-- daylight savings time i.e. parsing will fail on e.g. EDT, NZDT. Ideally we
-- would fix this upstream, though in the meantime we leave out timezone info
-- altogether.

-- | Represents a point in time. The 'Eq'/'Ord'/'Hashable' instance is
-- determined by an equivalence class on its @YYYY-mm-dd H:M:S@
-- representation.
--
-- @since 0.1
newtype Timestamp = MkTimestamp LocalTime
  deriving stock
    ( -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance Eq Timestamp where
  x == y = toStr x == toStr y

-- | @since 0.1
instance Ord Timestamp where
  x <= y = toStr x <= toStr y

-- | @since 0.1
instance Hashable Timestamp where
  hashWithSalt s = hashWithSalt s . toStr

-- | @since 0.1
instance Pretty Timestamp where
  pretty = fromString . toStr

-- | @since 0.1
_MkTimestamp :: Iso' Timestamp LocalTime
_MkTimestamp = iso (\(MkTimestamp x) -> x) MkTimestamp

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
  toField = str2Bs . toStr

-- | Retrieves the local time.
--
-- @since 0.1
getCurrentLocalTime :: IO Timestamp
getCurrentLocalTime =
  MkTimestamp . Local.zonedTimeToLocalTime <$> Local.getZonedTime

toStr :: Timestamp -> String
toStr =
  -- NOTE: If we ever handle timezones we will want to change this locale
  -- to one that handles more timezones (e.g see time-conv)
  Format.formatTime Format.defaultTimeLocale format
    . view _MkTimestamp

format :: String
format = "%Y-%m-%d %H:%M:%S"

bs2Str :: ByteString -> String
bs2Str = Char8.unpack . Char8.strip

str2Bs :: String -> ByteString
str2Bs = Char8.strip . Char8.pack
