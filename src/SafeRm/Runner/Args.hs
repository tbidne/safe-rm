{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides CLI args functionality.
--
-- @since 0.1
module SafeRm.Runner.Args
  ( getArgs,
    Args (..),
    TomlConfigPath (..),
    _TomlNone,
    _TomlDefault,
    _TomlPath,
  )
where

import Control.Applicative qualified as A
import Data.List qualified as L
import Data.Version.Package qualified as PV
import Development.GitRev qualified as GitRev
import Options.Applicative
  ( CommandFields,
    InfoMod,
    Mod,
    Parser,
    ParserInfo
      ( ParserInfo,
        infoFailureCode,
        infoFooter,
        infoFullDesc,
        infoHeader,
        infoParser,
        infoPolicy,
        infoProgDesc
      ),
    (<**>),
  )
import Options.Applicative qualified as OA
import Options.Applicative.Help.Chunk (Chunk (Chunk))
import Options.Applicative.Types (ArgPolicy (Intersperse))
import SafeRm.Data.Paths (PathI, PathIndex (TrashHome))
import SafeRm.Data.UniqueSeq (UniqueSeq, fromFoldable)
import SafeRm.Effects.MonadLoggerContext qualified as Logger
import SafeRm.Prelude
import SafeRm.Runner.Command
  ( Command
      ( Delete,
        DeletePerm,
        Empty,
        List,
        Metadata,
        Restore
      ),
  )

-- | Toml path config.
--
-- @since 0.1
data TomlConfigPath
  = -- | Do not use any Toml config.
    --
    -- @since 0.1
    TomlNone
  | -- | Attempts to read the Toml file at the default path.
    --
    -- @since 0.1
    TomlDefault
  | -- | Path to Toml file.
    --
    -- @since 0.1
    TomlPath !FilePath
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

makePrisms ''TomlConfigPath

-- | CLI args.
--
-- @since 0.1
data Args = MkArgs
  { -- | Path to toml config.
    --
    -- @since 0.1
    tomlConfigPath :: !TomlConfigPath,
    -- | Path to trash home.
    --
    -- @since 0.1
    trashHome :: !(Maybe (PathI TrashHome)),
    -- | The file logging level.
    --
    -- @since 0.1
    logLevel :: !(Maybe (Maybe LogLevel)),
    -- | Command to run.
    --
    -- @since 0.1
    command :: !Command
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )

makeFieldLabelsNoPrefix ''Args

-- | Retrieves CLI args.
--
-- @since 0.1
getArgs :: IO Args
getArgs = OA.execParser parserInfoArgs

parserInfoArgs :: ParserInfo Args
parserInfoArgs =
  ParserInfo
    { infoParser = argsParser,
      infoFullDesc = True,
      infoProgDesc = Chunk desc,
      infoHeader = Chunk headerTxt,
      infoFooter = Chunk footerTxt,
      infoFailureCode = 1,
      infoPolicy = Intersperse
    }
  where
    headerTxt = Just "Safe-rm: A tool for deleting files to a trash directory."
    footerTxt = Just $ fromString versNum
    desc =
      Just $
        mconcat
          [ "\nSafe-rm moves files to a trash directory, so they can later be ",
            "restored or permanently deleted. It is intended as a safer ",
            "alternative to rm. See github.com/tbidne/safe-rm#readme for ",
            "full documentation."
          ]

argsParser :: Parser Args
argsParser =
  MkArgs
    <$> configParser
    <*> trashParser
    <*> logLevelParser
    <*> commandParser
    <**> OA.helper
    <**> version

version :: Parser (a -> a)
version = OA.infoOption txt (OA.long "version")
  where
    txt =
      L.intercalate
        "\n"
        [ "SafeRm",
          versNum,
          "Revision: " <> $(GitRev.gitHash),
          "Date: " <> $(GitRev.gitCommitDate)
        ]

versNum :: String
versNum = "Version: " <> $$(PV.packageVersionStringTH "safe-rm.cabal")

configParser :: Parser TomlConfigPath
configParser =
  OA.option
    readTomlPath
    $ mconcat
      [ OA.value TomlDefault,
        OA.long "config",
        OA.short 'c',
        OA.metavar "[none|PATH]",
        OA.help helpTxt
      ]
  where
    helpTxt =
      mconcat
        [ "Path to the toml config file. Can be the string 'none' -- in which ",
          "case no toml config is used -- or a path to the config file. If ",
          "not specified then we look in the xdg config directory ",
          "e.g. ~/.config/safe-rm/config.toml"
        ]
    readTomlPath =
      OA.str >>= \case
        "none" -> pure TomlNone
        path -> pure $ TomlPath path

commandParser :: Parser Command
commandParser =
  OA.hsubparser
    ( mconcat
        [ mkCommand "d" delParser delTxt,
          mkCommand "x" permDelParser permDelTxt,
          mkCommand "e" emptyParser emptyTxt,
          OA.commandGroup "Delete Commands"
        ]
    )
    <|> OA.hsubparser
      ( mconcat
          [ mkCommand "r" restoreParser restoreTxt,
            OA.commandGroup "Restore Commands",
            OA.hidden
          ]
      )
    <|> OA.hsubparser
      ( mconcat
          [ mkCommand "l" listParser listTxt,
            mkCommand "m" metadataParser metadataTxt,
            OA.commandGroup "Information Commands",
            OA.hidden
          ]
      )
  where
    delTxt = OA.progDesc "Moves the path(s) to the trash."
    permDelTxt = OA.progDesc "Permanently deletes path(s) from the trash."
    emptyTxt = OA.progDesc "Empties the trash and deletes the index."
    restoreTxt =
      OA.progDesc
        "Restores the trash path(s) to their original location."
    listTxt = OA.progDesc "Lists all trash contents and metadata."
    metadataTxt = OA.progDesc "Prints trash metadata."

    delParser = Delete <$> pathsParser
    permDelParser =
      DeletePerm
        <$> forceParser
        <*> pathsParser
    emptyParser = Empty <$> forceParser
    restoreParser = Restore <$> pathsParser
    listParser = pure List
    metadataParser = pure Metadata

forceParser :: Parser Bool
forceParser =
  OA.switch $
    mconcat
      [ OA.long "force",
        OA.short 'f',
        OA.help helpTxt
      ]
  where
    helpTxt = "If enabled, will not ask before deleting each path."

trashParser :: Parser (Maybe (PathI TrashHome))
trashParser =
  A.optional
    $ OA.option
      OA.str
    $ mconcat
      [ OA.long "trash-home",
        OA.short 't',
        OA.metavar "PATH",
        OA.help helpTxt
      ]
  where
    helpTxt =
      mconcat
        [ "Path to the trash directory. This overrides the toml config, if ",
          "it exists. If neither is given then we use the xdg home directory ",
          "e.g. ~/.trash"
        ]

logLevelParser :: Parser (Maybe (Maybe LogLevel))
logLevelParser =
  A.optional $
    OA.option (OA.str >>= Logger.readLogLevel) $
      mconcat
        [ OA.long "log-level",
          OA.metavar Logger.logLevelStrings,
          OA.help "The file level in which to log. Defaults to none."
        ]

consoleLogLevelParser :: Parser (Maybe (Maybe LogLevel))
consoleLogLevelParser =
  A.optional $
    OA.option (OA.str >>= Logger.readLogLevel) $
      mconcat
        [ OA.long "console-log",
          OA.metavar Logger.logLevelStrings,
          OA.help "The console level in which to log. Defaults to error."
        ]

pathsParser :: (Hashable a, IsString a) => Parser (UniqueSeq a)
pathsParser =
  -- NOTE: _should_ be safe because OA.some only succeeds for non-zero input.
  -- We do this rather than using NonEmpty's some1 because otherwise the CLI
  -- help metavar is duplicated i.e. "PATHS... [PATHS...]".
  fromFoldable . unsafeNE
    <$> OA.some (OA.argument OA.str (OA.metavar "PATHS..."))

mkCommand :: String -> Parser a -> InfoMod a -> Mod CommandFields a
mkCommand cmdTxt parser helpTxt = OA.command cmdTxt (OA.info parser helpTxt)

unsafeNE :: HasCallStack => [a] -> NonEmpty a
unsafeNE [] = error "Args: Empty list given to unsafeNE"
unsafeNE (x : xs) = x :| xs
