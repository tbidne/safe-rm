-- | Provides internal utility functions
--
-- @since 0.1
module SafeRm.Utils
  ( whenJust,
    concatMNonEmpty,
    prependMNonEmpty,
    allM1,
  )
where

import SafeRm.Prelude

whenJust :: Applicative f => Maybe t -> (t -> f ()) -> f ()
whenJust Nothing _ = pure ()
whenJust (Just x) f = f x

concatMNonEmpty :: [a] -> Maybe (NonEmpty a) -> Maybe (NonEmpty a)
concatMNonEmpty [] ne = ne
concatMNonEmpty (x : xs) Nothing = Just $ x :| xs
concatMNonEmpty (x : xs) (Just (y :| ys)) = Just $ x :| xs <> (y : ys)

prependMNonEmpty :: a -> Maybe (NonEmpty a) -> Maybe (NonEmpty a)
prependMNonEmpty x Nothing = Just (x :| [])
prependMNonEmpty x (Just (y :| ys)) = Just (x :| y : ys)

-- | 'allM' that must have at least one 'True'.
--
-- @since 0.1
allM1 :: Monad m => NonEmpty (m Bool) -> m Bool
allM1 (m :| ms) =
  m >>= \case
    True -> allM ms
    False -> pure False

-- | 'Prelude.all' lifted to monads.
--
-- @since 0.1
allM :: (Foldable t, Monad m) => t (m Bool) -> m Bool
allM = foldr f (pure True)
  where
    f m acc =
      m >>= \case
        True -> acc
        False -> pure False
