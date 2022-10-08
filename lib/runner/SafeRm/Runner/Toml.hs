-- | Provides TOML configuration.
--
-- @since 0.1
module SafeRm.Runner.Toml
  ( TomlConfig (..),
    mergeConfigs,
  )
where

import SafeRm.Data.Paths (PathI (MkPathI), PathIndex (TrashHome))
import SafeRm.Effects.Logger (LogLevel, readLogLevel)
import SafeRm.Prelude
import SafeRm.Runner.Args (Args (consoleLogLevel, fileLogLevel, trashHome))
import TOML
  ( DecodeTOML (..),
    getFieldOptWith,
  )

-- | Holds TOML configuration.
--
-- @since 0.1
data TomlConfig = MkTomlConfig
  { trashHome :: !(Maybe (PathI TrashHome)),
    consoleLogLevel :: !(Maybe LogLevel),
    fileLogLevel :: !(Maybe LogLevel)
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Semigroup TomlConfig where
  MkTomlConfig a b c <> MkTomlConfig a' b' c' =
    MkTomlConfig (a <|> a') (b <|> b') (c <|> c')

-- | @since 0.1
instance Monoid TomlConfig where
  mempty = MkTomlConfig Nothing Nothing Nothing

-- | @since 0.5
instance DecodeTOML TomlConfig where
  tomlDecoder =
    MkTomlConfig
      <$> decodeTrashHome
      <*> decodeLogLevel "console"
      <*> decodeLogLevel "file"
    where
      decodeTrashHome =
        fmap MkPathI <$> getFieldOptWith tomlDecoder "trash-home"
      decodeLogLevel s =
        getFieldOptWith (tomlDecoder >>= readLogLevel) (s <> "-log-level")

-- | Merges the args and toml config into a single toml config. If some field
-- F is specified by both args and toml config, then args takes precedence.
--
-- @since 0.1
mergeConfigs :: Args -> TomlConfig -> TomlConfig
mergeConfigs args = (argsToTomlConfig args <>)

argsToTomlConfig :: Args -> TomlConfig
argsToTomlConfig args =
  MkTomlConfig
    { trashHome = args ^. #trashHome,
      fileLogLevel = args ^. #fileLogLevel,
      consoleLogLevel = args ^. #consoleLogLevel
    }
