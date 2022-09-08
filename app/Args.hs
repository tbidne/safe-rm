{-# LANGUAGE TemplateHaskell #-}

-- | Provides CLI args functionality.
--
-- @since 0.1
module Args
  ( getArgs,
    Args (MkArgs, paths, restore),
  )
where

import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.String (IsString (fromString))
import Data.Version.Package qualified as PV
import Development.GitRev qualified as GitRev
import Options.Applicative
  ( Parser,
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
data Action
  = -- | Deletes a path.
    --
    -- @since 0.1
    ActionDelete
  | -- | Restores a path.
    --
    -- @since 0.1
    ActionRestore
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
  { -- | List of paths to move to the trash.
    --
    -- @since 0.1
    restore :: !Bool,
    -- | List of paths to move to the trash.
    --
    -- @since 0.1
    paths :: !(NonEmpty FilePath)
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
    <$> restoreParser
    <*> pathsParser
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

restoreParser :: Parser Bool
restoreParser =
  OA.flag
    False
    True
    $ mconcat
      [ OA.short 'r',
        OA.long "restore",
        OA.help helpTxt
      ]
  where
    helpTxt = "Restores a path from the trash."

pathsParser :: Parser (NonEmpty FilePath)
pathsParser =
  NE.some1 (OA.argument OA.str (OA.metavar "PATHS..."))
