-- | Provides CLI args functionality.
--
-- @since 0.1
module Args
  ( getArgs,
    Args (MkArgs, paths),
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
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
  )
import Options.Applicative qualified as OA
import Options.Applicative.Help.Chunk (Chunk (Chunk))
import Options.Applicative.Types (ArgPolicy (Intersperse))

-- | Retrieves CLI args.
--
-- @since 0.1
getArgs :: IO Args
getArgs = OA.execParser parserInfoArgs

-- | CLI args.
--
-- @since 0.1
newtype Args = MkArgs
  { -- | List of paths to move to the trash.
    --
    -- @since 0.1
    paths :: NonEmpty FilePath
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
    -- footerTxt = Just $ fromString versNum
    footerTxt = Nothing
    desc = Nothing

argsParser :: Parser Args
argsParser =
  MkArgs <$> pathsParser

pathsParser :: Parser (NonEmpty FilePath)
pathsParser =
  NE.some1 (OA.argument OA.str (OA.metavar "PATHS..."))
