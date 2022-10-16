-- | Prelude for unit test suite.
--
-- @since 0.1
module Unit.Prelude
  ( module X,
    getDefaultTrash,
  )
where

import Hedgehog as X
  ( Gen,
    MonadGen,
    MonadTest,
    Property,
    annotate,
    annotateShow,
    assert,
    failure,
    forAll,
    property,
    withTests,
    (===),
  )
import SafeRm.MaxRuns as X (MaxRuns (MkMaxRuns), maxRunsIngredient)
import SafeRm.Prelude as X
import Test.Tasty as X (TestTree, askOption, testGroup)
import Test.Tasty.HUnit as X
  ( assertBool,
    assertEqual,
    assertFailure,
    testCase,
    (@=?),
  )
import Test.Tasty.Hedgehog as X (testPropertyNamed)
import UnliftIO.Directory qualified as Dir

getDefaultTrash :: IO FilePath
getDefaultTrash = (</> ".trash") <$> Dir.getHomeDirectory
