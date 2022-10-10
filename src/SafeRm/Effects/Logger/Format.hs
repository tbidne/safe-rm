-- | Provides utilities for formatting logs.
--
-- @since 0.1
module SafeRm.Effects.Logger.Format
  ( LogFormat (..),
    LogLoc (..),
    formatLog,
  )
where

import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Language.Haskell.TH.Syntax (Loc)
import Language.Haskell.TH.Syntax qualified as THS
import SafeRm.Data.Timestamp qualified as Timestamp
import SafeRm.Effects.Logger.Types
  ( LogLevel,
  )
import SafeRm.Prelude

-- | Options for logging location data.
--
-- @since 0.1
data LogLoc
  = -- | Include verbose 'Loc' info.
    --
    -- @since 0.1
    LogLocFull Loc
  | -- | Less noise 'Loc' info, module and line/col number only.
    --
    -- @since 0.1
    LogLocPartial Loc
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )

-- | Options for formatting logs.
--
-- @since 0.1
data LogFormat = MkLogFormat
  { -- | How to handle the 'Loc' data, if any.
    --
    -- @since 0.1
    logLoc :: !(Maybe LogLoc),
    -- | Namespace for this log, if any.
    --
    -- @since 0.1
    namespace :: !(Maybe (Seq Text)),
    -- | Whether to include a timestamp.
    --
    -- @since 0.1
    withTimestamp :: !Bool,
    -- | Whether to add a newline.
    --
    -- @since 0.1
    newline :: !Bool
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
instance Semigroup LogFormat where
  MkLogFormat a b c d <> MkLogFormat a' b' c' d' =
    MkLogFormat (a <|> a') (b <|> b') (c || c') (d || d')

-- | @since 0.1
instance Monoid LogFormat where
  mempty = MkLogFormat empty empty False False

-- | Formats the given message.
--
-- @since 0.1
formatLog :: MonadIO m => LogFormat -> LogLevel -> Text -> m Text
formatLog fmt lvl message = do
  ts <-
    if fmt ^. #withTimestamp
      then withBrackets . Timestamp.toText <$> Timestamp.getCurrentLocalTime
      else pure ""

  let logLocTxt = case fmt ^. #logLoc of
        Just (LogLocFull loc) -> withBrackets $ fullLoc loc
        Just (LogLocPartial loc) -> withBrackets $ partialLoc loc
        Nothing -> ""
      namespaceTxt = case fmt ^. #namespace of
        Nothing -> ""
        Just namespace -> withBrackets $ foldMap' id $ Seq.intersperse "." namespace
      newline
        | fmt ^. #newline = "\n"
        | otherwise = ""

      formatted =
        mconcat
          [ ts,
            namespaceTxt,
            withBrackets (showt lvl),
            logLocTxt,
            " ",
            message,
            newline
          ]
  pure formatted
  where
    withBrackets s = "[" <> s <> "]"

    fullLoc loc =
      mconcat
        [ T.pack $ view #loc_package loc,
          T.cons ':' (T.pack $ view #loc_module loc),
          T.cons ' ' (partialLoc loc)
        ]
    partialLoc loc =
      mconcat
        [ T.pack $ view #loc_filename loc,
          T.cons ':' (mkLine loc),
          T.cons ':' (mkChar loc)
        ]

    mkLine = showt . view (#loc_start % _1)
    mkChar = showt . view (#loc_start % _2)
