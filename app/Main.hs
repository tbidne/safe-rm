-- | Main module.
--
-- @since 0.1
module Main (main) where

import GHC.Conc.Sync (setUncaughtExceptionHandler)
import SafeRm.Prelude (displayCallStack)
import SafeRm.Runner (runSafeRm)

main :: IO ()
main = do
  setUncaughtExceptionHandler (putStrLn . displayCallStack)
  runSafeRm
