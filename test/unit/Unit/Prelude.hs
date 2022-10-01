-- | Prelude for unit test suite.
--
-- @since 0.1
module Unit.Prelude
  ( module X,

    -- * Assertions
    assertFilesExist,
    assertFilesDoNotExist,
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
    forAll,
    property,
    withTests,
    (===),
  )
import SafeRm.FileUtils as X
import SafeRm.Prelude as X
import Test.Tasty as X (TestTree, askOption, testGroup)
import Test.Tasty.Hedgehog as X (testPropertyNamed)
import UnliftIO.Directory qualified as Dir

-- | Asserts that files exist.
--
-- @since 0.1
assertFilesExist :: (MonadIO m, MonadTest m) => [FilePath] -> m ()
assertFilesExist paths =
  for_ paths $ \p -> do
    exists <- liftIO $ Dir.doesFileExist p
    annotate $ "Expected file to exist: " <> p
    assert exists

-- | Asserts that files do not exist.
--
-- @since 0.1
assertFilesDoNotExist :: (MonadIO m, MonadTest m) => [FilePath] -> m ()
assertFilesDoNotExist paths =
  for_ paths $ \p -> do
    exists <- liftIO $ Dir.doesFileExist p
    annotate $ "Expected file not to exist: " <> p
    assert (not exists)
