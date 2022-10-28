-- | Main module.
--
-- @since 0.1
module Main (main) where

import Control.Exception (Exception (displayException))
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import SafeRm.Runner (runSafeRm)

main :: IO ()
main = do
  -- NOTE: Using setUncaughtExceptionHandler means we do not have to manually
  -- catch exceptions and print ourselves, just to get nicer output from
  -- show.
  setUncaughtExceptionHandler (putStrLn . displayException)
  runSafeRm
