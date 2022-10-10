{-# LANGUAGE TemplateHaskell #-}

-- | Provides the 'Logger' typeclass.
--
-- @since 0.1
module SafeRm.Effects.Logger
  ( -- * Types

    -- ** Class
    Logger (..),

    -- ** Data
    LogContext (..),
    LogLevel (..),
    readLogLevel,

    -- * Functions

    -- ** TH
    logErrorTH,
    logWarnTH,
    logInfoTH,
    logDebugTH,
    logShowTH,
    logExceptionTH,

    -- ** Non-TH
    logErrorM,
    logWarnM,
    logInfoM,
    logDebugM,
    logShowM,
    logExceptionM,

    -- ** Low level
    logM,
    logTH,
  )
where

import Data.Text qualified as T
import Language.Haskell.TH.Syntax (Exp, Loc (Loc), Q, qLocation)
import Language.Haskell.TH.Syntax qualified as THS
import SafeRm.Effects.Logger.Types
import SafeRm.Prelude

-- | Logs 'Error' with no location.
--
-- @since 0.1
logErrorM :: (Logger m, MonadIO m) => Text -> m ()
logErrorM = logM Error

-- | Logs 'Error' with location.
--
-- @since 0.1
logErrorTH :: Q Exp
logErrorTH = logTH Error

-- | Logs 'Warn' without location.
--
-- @since 0.1
logWarnM :: (Logger m, MonadIO m) => Text -> m ()
logWarnM = logM Warn

-- | Logs 'Warn' with location.
--
-- @since 0.1
logWarnTH :: Q Exp
logWarnTH = logTH Warn

-- | Logs 'Info' without location.
--
-- @since 0.1
logInfoM :: (Logger m, MonadIO m) => Text -> m ()
logInfoM = logM Info

-- | Logs 'Info' with location.
--
-- @since 0.1
logInfoTH :: Q Exp
logInfoTH = logTH Info

--- | Logs 'Debug' without location.
--
-- @since 0.1
logDebugM :: (Logger m, MonadIO m) => Text -> m ()
logDebugM = logM Debug

-- | Logs 'Debug' with location.
--
-- @since 0.1
logDebugTH :: Q Exp
logDebugTH = logTH Debug

-- | Logs 'Show' without location.
--
-- @since 0.1
logShowM :: (Logger m, MonadIO m, Show a) => LogLevel -> a -> m ()
logShowM lvl = logM lvl . showt

-- | Logs 'Show' with location.
--
-- @since 0.1
logShowTH :: LogLevel -> Q Exp
logShowTH lvl = [|log' (Just $(qLocation >>= liftLoc)) $(THS.lift lvl) . showt|]

-- | Logs 'Exception' without location.
--
-- @since 0.1
logExceptionM :: (Exception e, Logger m, MonadIO m) => LogLevel -> e -> m ()
logExceptionM lvl = logM lvl . T.pack . displayException

-- | Logs 'Exception' with location.
--
-- @since 0.1
logExceptionTH :: LogLevel -> Q Exp
logExceptionTH lvl =
  [|
    log' (Just $(qLocation >>= liftLoc)) $(THS.lift lvl)
      . T.pack
      . displayException
    |]

-- | Logs without location.
--
-- @since 0.1
logM :: (Logger m, MonadIO m) => LogLevel -> Text -> m ()
logM = log' Nothing

-- | Logs with location.
--
-- @since 0.1
logTH :: LogLevel -> Q Exp
-- NOTE: can't use typed TH because it does not handle constraints correctly.
logTH lvl = [|log' (Just $(qLocation >>= liftLoc)) $(THS.lift lvl)|]

log' :: (Logger m, MonadIO m) => Maybe Loc -> LogLevel -> Text -> m ()
log' mloc lvl txt = do
  ctx <- getContext
  let namespace = ctx ^. #namespace
  for_ (ctx ^. #scribes) $ \(MkScribe logFn scribeLvl) -> do
    when (lvl <= scribeLvl) $ liftIO $ logFn namespace mloc lvl txt

liftLoc :: Loc -> Q Exp
liftLoc (Loc a b c (d1, d2) (e1, e2)) =
  [|
    Loc
      $(THS.lift a)
      $(THS.lift b)
      $(THS.lift c)
      ($(THS.lift d1), $(THS.lift d2))
      ($(THS.lift e1), $(THS.lift e2))
    |]
