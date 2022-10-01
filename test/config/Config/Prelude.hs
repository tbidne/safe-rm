-- | Prelude for functional test suite.
--
-- @since 0.1
module Config.Prelude
  ( module X,
  )
where

import SafeRm.Prelude as X
import Test.Tasty as X (TestTree, testGroup)
import Test.Tasty.HUnit as X
  ( assertBool,
    assertEqual,
    assertFailure,
    testCase,
    (@=?),
  )
