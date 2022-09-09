{-# LANGUAGE TemplateHaskell #-}

-- | Provides CLI args functionality.
--
-- @since 0.1
module Args
  ( getArgs,
    Args (..),
    DelCommand (..),
  )
where

import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.String (IsString (fromString))
import Data.Version.Package qualified as PV
import Development.GitRev qualified as GitRev
import GHC.Stack (HasCallStack)
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
    DelCommandDelete !(NonEmpty FilePath)
  | -- | Restores a path.
    --
    -- @since 0.1
    DelCommandRestore !(NonEmpty FilePath)
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | CLI args.
--
-- @since 0.1
data Args = MkArgs
  { -- | Command to run.
    --
    -- @since 0.1
    command :: !DelCommand
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
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
versNum = "Version: " <> $$(PV.packageVersionStringTH "dir.cabal")

commandParser :: Parser DelCommand
commandParser =
  OA.hsubparser $
    mconcat
      [ mkCommand "d" (DelCommandDelete <$> pathsParser) delTxt,
        mkCommand "r" (DelCommandRestore <$> pathsParser) restoreTxt
      ]
  where
    delTxt = OA.progDesc "Moves the path to the trash."
    restoreTxt = OA.progDesc "Restores the trash path to its original location."

pathsParser :: Parser (NonEmpty FilePath)
pathsParser =
  -- NOTE: _should_ be safe because OA.show only succeeds for non-zero input.
  -- We do this rather than using NonEmpty's some1 because otherwise the CLI
  -- help metavar is duplicated i.e. "PATHS... [PATHS...]".
  unsafeNE
    <$> OA.some (OA.argument OA.str (OA.metavar "PATHS..."))

mkCommand :: String -> Parser a -> InfoMod a -> Mod CommandFields a
mkCommand cmdTxt parser helpTxt = OA.command cmdTxt (OA.info parser helpTxt)

unsafeNE :: HasCallStack => [a] -> (NonEmpty a)
unsafeNE [] = error "Args: Empty list given to unsafeNE"
unsafeNE (x : xs) = x :| xs
