-- | Provides internal utility functions
--
-- @since 0.1
module Del.Utils
  ( allM1,
  )
where

import Del.Prelude

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
