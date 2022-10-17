-- | Entrypoint for unit tests.
--
-- @since 0.1
module Main (main) where

import Test.Tasty qualified as Tasty
import Unit.Data.UniqueSeq qualified as UniqueSeq
import Unit.Prelude
import Unit.Runner qualified as Runner

-- | Runs unit tests.
--
-- @since 0.1
main :: IO ()
main =
  Tasty.defaultMain $
    testGroup
      "Unit Tests"
      [ Runner.tests,
        UniqueSeq.tests
      ]
