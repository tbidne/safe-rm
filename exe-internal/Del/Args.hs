{-# LANGUAGE TemplateHaskell #-}

-- | Provides CLI args functionality.
--
-- @since 0.1
module Del.Args
  ( getArgs,
    Args (..),
    DelCommand (..),
  )
where

import Control.Applicative qualified as A
import Data.List qualified as L
import Data.Version.Package qualified as PV
import Del.Data.Paths (PathI, PathIndex (..))
import Del.Prelude
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

-- | Retrieves CLI args.
--
-- @since 0.1
getArgs :: IO Args
getArgs = OA.execParser parserInfoArgs

-- | Action to run.
--
-- @since 0.1
data DelCommand
  = -- | Deletes a path.
    --
    -- @since 0.1
    DelCommandDelete
      !(Maybe (PathI TrashHome))
      !(NonEmpty (PathI OriginalName))
  | -- | Permanently deletes a path from the trash.
    --
    -- @since 0.1
    DelCommandPermDelete
      !(Maybe (PathI TrashHome))
      !Bool
      !(NonEmpty (PathI TrashName))
  | -- | Empties the trash.
    --
    -- @since 0.1
    DelCommandEmpty !(Maybe (PathI TrashHome))
  | -- | Restores a path.
    --
    -- @since 0.1
    DelCommandRestore !(Maybe (PathI TrashHome)) !(NonEmpty (PathI TrashName))
  | -- | List all trash contents.
    --
    -- @since 0.1
    DelCommandList !(Maybe (PathI TrashHome))
  | -- | Prints trash size.
    --
    -- @since 0.1
    DelCommandMetadata !(Maybe (PathI TrashHome))
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | CLI args.
--
-- @since 0.1
newtype Args = MkArgs
  { -- | Command to run.
    --
    -- @since 0.1
    command :: DelCommand
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
    headerTxt = Just "Del: A tool for deleting files to a trash directory."
    footerTxt = Just $ fromString versNum
    desc =
      Just $
        mconcat
          [ "\nDel moves files to a trash directory, so they later can be ",
            "restored or permanently deleted. It is intended as a safer ",
            "alternative to e.g. rm."
          ]

argsParser :: Parser Args
argsParser =
  MkArgs
    <$> commandParser
    <**> OA.helper
    <**> version

version :: Parser (a -> a)
version = OA.infoOption txt (OA.long "version" <> OA.short 'v')
  where
    txt =
      L.intercalate
        "\n"
        [ "Del",
          versNum,
          "Revision: " <> $(GitRev.gitHash),
          "Date: " <> $(GitRev.gitCommitDate)
        ]

versNum :: String
versNum = "Version: " <> $$(PV.packageVersionStringTH "del.cabal")

commandParser :: Parser DelCommand
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
      DelCommandDelete
        <$> trashParser
        <*> pathsParser
    permDelParser =
      DelCommandPermDelete
        <$> trashParser
        <*> forceParser
        <*> pathsParser
    emptyParser = DelCommandEmpty <$> trashParser
    restoreParser =
      DelCommandRestore
        <$> trashParser
        <*> pathsParser
    listParser = DelCommandList <$> trashParser
    metadataParser = DelCommandMetadata <$> trashParser

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
      [ OA.long "trash",
        OA.short 't',
        OA.metavar "PATH",
        OA.help helpTxt
      ]
  where
    helpTxt =
      mconcat
        [ "Path to the trash directory. If none is given we default to ",
          "XDG/.trash e.g. ~/.trash."
        ]

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
