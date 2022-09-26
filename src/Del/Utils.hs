-- | Provides internal utility functions
--
-- @since 0.1
module Del.Utils
  ( -- * Default trash/index
    getTrashAndIndex,

    -- * Utilities
    allM1,
  )
where

import Del.Prelude
import System.Directory qualified as Dir

-- | Returns @(trashHome, indexPath)@, where @trashHome@ is either the
-- user-supplied path or the default, if none is given.
--
-- @since 0.1
getTrashAndIndex :: Maybe FilePath -> IO (FilePath, FilePath)
getTrashAndIndex mfp = do
  res <- trashOrDefault mfp
  pure (res, res </> ".index.csv")

-- | If the argument is given, returns it. Otherwise searches for the default
-- trash location.
--
-- @since 0.1
trashOrDefault :: Maybe FilePath -> IO FilePath
trashOrDefault = maybe getTrashHome pure

-- | Retrieves the default trash directory.
--
-- @since 0.1
getTrashHome :: IO FilePath
getTrashHome = (</> ".trash") <$> Dir.getHomeDirectory

-- | 'allM' that must have at least one 'True'.
--
-- @since 0.1
allM1 :: Monad m => NonEmpty (m Bool) -> m Bool
allM1 (m :| ms) =
  m >>= \case
    True -> allM ms
    False -> pure False

-- | 'all' lifted to monads.
--
-- @since 0.1
allM :: (Foldable t, Monad m) => t (m Bool) -> m Bool
allM = foldr f (pure True)
  where
    f m acc =
      m >>= \case
        True -> acc
        False -> pure False
