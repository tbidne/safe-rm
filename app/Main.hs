-- | Main module.
--
-- @since 0.1
module Main (main) where

import SafeRm.Runner (runSafeRm)

main :: IO ()
-- TODO: translate any exceptions here to exitFailure. Runner is already
-- logging them.
main = runSafeRm
