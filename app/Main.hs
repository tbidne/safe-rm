-- | Main module.
--
-- @since 0.1
module Main (main) where

import SafeRm.Runner (runSafeRm)
import System.Exit (exitFailure)
import UnliftIO.Exception (Exception (displayException), catchAny)

main :: IO ()
main =
  runSafeRm `catchAny` \e -> do
    -- NOTE: catches anything missed by the runner
    -- (e.g. setup errors are not caught)
    putStrLn ("Exception: " <> displayException e)
    exitFailure
