-- | Main module.
--
-- @since 0.1
module Main (main) where

import SafeRm.Runner (runSafeRm)
import System.Exit (exitFailure)
import UnliftIO.Exception (catchAny)

main :: IO ()
main = runSafeRm `catchAny` const exitFailure
