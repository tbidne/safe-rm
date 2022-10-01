-- | Entrypoint for config tests.
--
-- @since 0.1
module Main (main) where

import Config.Args qualified as Args
import Config.Prelude
import Config.Toml qualified as Toml
import Test.Tasty qualified as Tasty

-- | Runs config tests.
--
-- @since 0.1
main :: IO ()
main =
  Tasty.defaultMain $
    testGroup
      "Config Tests"
      [ Args.tests,
        Toml.tests
      ]
