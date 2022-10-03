{-# LANGUAGE TemplateHaskell #-}

-- | Provides CLI args functionality.
--
-- @since 0.1
module SafeRm.Args
  ( getArgs,
    Args (..),
    SafeRmCommand (..),
    _SafeRmCommandDelete,
    _SafeRmCommandPermDelete,
    _SafeRmCommandEmpty,
    _SafeRmCommandRestore,
    _SafeRmCommandList,
    _SafeRmCommandMetadata,
  )
where

import Control.Applicative qualified as A
import Data.List qualified as L
import Data.Version.Package qualified as PV
import Development.GitRev qualified as GitRev
import Optics.TH (makePrisms)
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
import SafeRm.Data.Paths
  ( PathI,
    PathIndex
      ( OriginalPath,
        TrashHome,
        TrashName
      ),
  )
import SafeRm.Prelude

-- | Action to run.
--
-- @since 0.1
data SafeRmCommand
  = -- | Deletes a path.
    --
    -- @since 0.1
    SafeRmCommandDelete !(NonEmpty (PathI OriginalPath))
  | -- | Permanently deletes a path from the trash.
    --
    -- @since 0.1
    SafeRmCommandPermDelete
      !Bool
      !(NonEmpty (PathI TrashName))
  | -- | Empties the trash.
    --
    -- @since 0.1
    SafeRmCommandEmpty !Bool
  | -- | Restores a path.
    --
    -- @since 0.1
    SafeRmCommandRestore (NonEmpty (PathI TrashName))
  | -- | List all trash contents.
    --
    -- @since 0.1
    SafeRmCommandList
  | -- | Prints trash size.
    --
    -- @since 0.1
    SafeRmCommandMetadata
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

makePrisms ''SafeRmCommand

-- | Retrieves CLI args.
--
-- @since 0.1
getArgs :: IO Args
getArgs = OA.execParser parserInfoArgs

-- | CLI args.
--
-- @since 0.1
data Args = MkArgs
  { -- | Path to toml config.
    --
    -- @since 0.1
    tomlConfigPath :: !(Maybe FilePath),
    -- | Path to trash home..
    --
    -- @since 0.1
    trashHome :: !(Maybe (PathI TrashHome)),
    -- | verbose
    --
    -- @since 0.1
    verbose :: !(Maybe Bool),
    -- | Command to run.
    --
    -- @since 0.1
    command :: !SafeRmCommand
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )

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
    <*> verboseParser
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

configParser :: Parser (Maybe FilePath)
configParser =
  A.optional
    $ OA.option
      OA.str
    $ mconcat
      [ OA.long "config",
        OA.short 'c',
        OA.metavar "PATH",
        OA.help helpTxt
      ]
  where
    helpTxt =
      mconcat
        [ "Path to the toml config file. If none is given we default to ",
          "the xdg config directory e.g. ~/.config/safe-rm/config.toml"
        ]

commandParser :: Parser SafeRmCommand
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

    delParser =
      SafeRmCommandDelete <$> pathsParser
    permDelParser =
      SafeRmCommandPermDelete
        <$> forceParser
        <*> pathsParser
    emptyParser = SafeRmCommandEmpty <$> forceParser
    restoreParser = SafeRmCommandRestore <$> pathsParser
    listParser = pure SafeRmCommandList
    metadataParser = pure SafeRmCommandMetadata

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

verboseParser :: Parser (Maybe Bool)
verboseParser =
  A.optional $
    OA.switch $
      mconcat
        [ OA.long "verbose",
          OA.short 'v',
          OA.help helpTxt
        ]
  where
    helpTxt = "Verbosity level e.g. if we print data that is deleted/restored."

pathsParser :: IsString a => Parser (NonEmpty a)
pathsParser =
  -- NOTE: _should_ be safe because OA.some only succeeds for non-zero input.
  -- We do this rather than using NonEmpty's some1 because otherwise the CLI
  -- help metavar is duplicated i.e. "PATHS... [PATHS...]".
  unsafeNE
    <$> OA.some (OA.argument OA.str (OA.metavar "PATHS..."))

mkCommand :: String -> Parser a -> InfoMod a -> Mod CommandFields a
mkCommand cmdTxt parser helpTxt = OA.command cmdTxt (OA.info parser helpTxt)

unsafeNE :: HasCallStack => [a] -> NonEmpty a
unsafeNE [] = error "Args: Empty list given to unsafeNE"
unsafeNE (x : xs) = x :| xs
