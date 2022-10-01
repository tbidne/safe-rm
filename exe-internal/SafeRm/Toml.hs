-- | Provides TOML configuration.
--
-- @since 0.1
module SafeRm.Toml
  ( TomlConfig (..),
    mergeConfigs,
  )
where

import SafeRm.Args (Args (trashHome, verbose))
import SafeRm.Data.Paths (PathI (MkPathI), PathIndex (TrashHome))
import SafeRm.Prelude
import TOML
  ( DecodeTOML (..),
    getFieldOptWith,
  )

-- | Holds TOML configuration.
--
-- @since 0.1
data TomlConfig = MkTomlConfig
  { trashHome :: !(Maybe (PathI TrashHome)),
    verbose :: !(Maybe Bool)
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
  MkTomlConfig a b <> MkTomlConfig a' b' = MkTomlConfig (a <|> a') (b <|> b')

-- | @since 0.1
instance Monoid TomlConfig where
  mempty = MkTomlConfig Nothing Nothing

-- | @since 0.5
instance DecodeTOML TomlConfig where
  tomlDecoder = MkTomlConfig <$> decodeTrashHome <*> decodeVerbose
    where
      decodeTrashHome =
        fmap MkPathI <$> getFieldOptWith tomlDecoder "trash-home"
      decodeVerbose =
        getFieldOptWith tomlDecoder "verbose"

-- | Merges the args and toml config into a single toml config. If some field
-- F is specified by both args and toml config, then args takes precedence.
--
-- @since 0.1
mergeConfigs :: Args -> TomlConfig -> TomlConfig
mergeConfigs args = (argsToTomlConfig args <>)

argsToTomlConfig :: Args -> TomlConfig
argsToTomlConfig args =
  MkTomlConfig (args ^. #trashHome) (args ^. #verbose)
