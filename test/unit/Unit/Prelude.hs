-- | Prelude for unit test suite.
--
-- @since 0.1
module Unit.Prelude
  ( module X,
    getDefaultTrash,
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
import UnliftIO.Directory qualified as Dir

getDefaultTrash :: IO FilePath
getDefaultTrash = (</> ".trash") <$> Dir.getHomeDirectory
