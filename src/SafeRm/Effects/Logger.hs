-- | Provides logging utilities.
--
-- @since 0.1
module SafeRm.Effects.Logger
  ( -- * Reading
    readLogLevel,
    logLevelStrings,

    -- * Formatting
    consoleFormatter,
    fileFormatter,
  )
where

import Data.Text qualified as T
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as TLB
import Katip (Environment, Item, Verbosity)
import Katip qualified as K
import Katip.Core (Verbosity (V0))
import Katip.Core qualified as KCore
import Katip.Format.Time qualified as KTime
import Katip.Scribes.Handle (ColorStrategy (ColorIfTerminal))
import Katip.Scribes.Handle qualified as KHandle
import Language.Haskell.TH (Loc (loc_filename, loc_start))
import SafeRm.Prelude
import System.IO qualified as IO

readLogLevel :: MonadFail m => Text -> m (Maybe Severity)
readLogLevel "none" = pure Nothing
readLogLevel "error" = pure $ Just ErrorS
readLogLevel "warn" = pure $ Just WarningS
readLogLevel "info" = pure $ Just InfoS
readLogLevel "debug" = pure $ Just DebugS
readLogLevel other =
  fail $
    mconcat
      [ "Expected log-level ",
        logLevelStrings,
        ", received: ",
        T.unpack other
      ]

logLevelStrings :: String
logLevelStrings = "[none|error|warn|info|debug]"

consoleFormatter :: Bool -> Verbosity -> Item a -> Builder
consoleFormatter _withColor _verb i =
  mconcat
    [ brackets (TLB.fromText (renderSeverity' (view #_itemSeverity i))),
      " ",
      view (#_itemMessage % #unLogStr) i
    ]
  where
    renderSeverity' severity =
      KHandle.colorBySeverity True severity (K.renderSeverity severity)
    brackets m = TLB.fromText "[" <> m <> TLB.fromText "]"

fileFormatter :: Bool -> Verbosity -> Item a -> Builder
fileFormatter _withColor _verb i =
  mconcat
    [ brackets nowStr,
      brackets (mconcat (TLB.fromText <$> KCore.intercalateNs (view #_itemNamespace i))),
      brackets (TLB.fromText (renderSeverity' (view #_itemSeverity i))),
      maybe mempty (brackets . partialLoc) (view #_itemLoc i),
      " ",
      view (#_itemMessage % #unLogStr) i
    ]
  where
    nowStr = TLB.fromText (KTime.formatAsLogTime (view #_itemTime i))
    renderSeverity' severity =
      KHandle.colorBySeverity False severity (K.renderSeverity severity)
    brackets m = TLB.fromText "[" <> m <> TLB.fromText "]"

partialLoc :: Loc -> Builder
partialLoc loc =
  mconcat
    [ TLB.fromString $ view #loc_filename loc,
      TLB.singleton ':' <> mkLine loc,
      TLB.singleton ':' <> mkChar loc
    ]
  where
    mkLine = TLB.fromString . show . view (#loc_start % _1)
    mkChar = TLB.fromString . show . view (#loc_start % _2)
