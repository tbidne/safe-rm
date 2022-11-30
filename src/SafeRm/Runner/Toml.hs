{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides TOML configuration.
--
-- @since 0.1
module SafeRm.Runner.Toml
  ( TomlConfig (..),
    mergeConfigs,
  )
where

import SafeRm.Data.PathData (PathDataFormat (Multiline, Singleline))
import SafeRm.Data.Paths (PathI (MkPathI), PathIndex (TrashHome))
import SafeRm.Prelude
import SafeRm.Runner.Args (Args, CommandArg (..), _ListArg)
import SafeRm.Runner.Command
import SafeRm.Runner.Config (CmdListCfg (..), ListFormatCfg (..))
import SafeRm.Utils qualified as U
import SafeRm.Utils qualified as Utils
import TOML
  ( DecodeTOML (..),
    getFieldOpt,
    getFieldOptWith,
  )

-- | Holds TOML configuration.
--
-- @since 0.1
data TomlConfig = MkTomlConfig
  { -- | Trash home.
    --
    -- @since 0.1
    trashHome :: !(Maybe (PathI TrashHome)),
    -- | Log level.
    --
    -- @since 0.1
    logLevel :: !(Maybe (Maybe LogLevel)),
    -- | List command configuration.
    --
    -- @since 0.1
    listCommand :: !(Maybe CmdListCfg)
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
makeFieldLabelsNoPrefix ''TomlConfig

-- | @since 0.1
instance Semigroup TomlConfig where
  MkTomlConfig a b c <> MkTomlConfig a' b' c' =
    MkTomlConfig
      -- Simple args, take the (left-biased) Just.
      (a <|> a')
      (b <|> b')
      -- For list config, use the semigroup instance because we want decisions
      -- at the _field_ level. The mere fact that the LHS is Just should
      -- _not_ cause it to completely overwrite the RHS.
      (c <> c')

-- | @since 0.1
instance Monoid TomlConfig where
  mempty = MkTomlConfig Nothing Nothing Nothing

-- | @since 0.5
instance DecodeTOML TomlConfig where
  tomlDecoder =
    MkTomlConfig
      <$> decodeTrashHome
      <*> decodeLogLevel
      <*> getFieldOptWith tomlDecoder "list"
    where
      decodeTrashHome = fmap MkPathI <$> getFieldOpt "trash-home"
      decodeLogLevel =
        getFieldOptWith (tomlDecoder >>= Utils.readLogLevel) "log-level"

-- | Merges the args and toml config into a single toml config. If some field
-- F is specified by both args and toml config, then args takes precedence.
--
-- Also updates the args' command with possible toml configuration.
--
-- @since 0.1
mergeConfigs :: Args -> TomlConfig -> (TomlConfig, Command)
mergeConfigs args toml = (mergedConfig, newCmd)
  where
    cmd = args ^. #command
    mergedConfig = argsToTomlConfig args <> toml
    newCmd = cmdFromToml mergedConfig cmd

argsToTomlConfig :: Args -> TomlConfig
argsToTomlConfig args =
  MkTomlConfig
    { trashHome = args ^. #trashHome,
      logLevel = args ^. #logLevel,
      listCommand = cmd ^? _ListArg
    }
  where
    cmd = args ^. #command

-- Returns the new command after possibly updating the old command from the
-- toml configuration.
cmdFromToml :: TomlConfig -> CommandArg -> Command
cmdFromToml toml = \case
  -- simple translations
  DeleteArg paths -> Delete paths
  DeletePermArg b paths -> DeletePerm b paths
  EmptyArg b -> Empty b
  RestoreArg paths -> Restore paths
  MetadataArg -> Metadata
  -- NOTE: The toml param contains config for the following explicitly listed
  -- commands. For these, use the toml rather than the command as it will
  -- have the most up-to-date config data (merged args + toml)
  (ListArg _) -> List $ U.maybeMonoid listCfgToCmd (toml ^. #listCommand)

listCfgToCmd :: CmdListCfg -> ListCommand
listCfgToCmd listCfg =
  MkListCommand
    { format,
      sort,
      revSort
    }
  where
    sort = U.fromMaybeMonoid (listCfg ^. #sort)
    revSort = fromMaybe False (listCfg ^. #revSort)
    format = case listCfg ^. #format of
      Just MultilineCfg -> Multiline
      -- default to singleline
      _ ->
        Singleline
          (fromMaybe 10 (listCfg ^. #nameTrunc))
          (fromMaybe 22 (listCfg ^. #origTrunc))
