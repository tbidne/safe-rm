-- | Provides internal utility functions
--
-- @since 0.1
module SafeRm.Utils
  ( whenJust,
    concatMNonEmpty,
    prependMNonEmpty,
    allM1,
    uniqueSeqFromList,
  )
where

import SafeRm.Prelude

-- | Applies the function when we have a Just.
--
-- @since 0.1
whenJust :: Applicative f => Maybe t -> (t -> f ()) -> f ()
whenJust Nothing _ = pure ()
whenJust (Just x) f = f x

-- | Concats the list with the non-empty.
--
-- @since 0.1
concatMNonEmpty :: [a] -> Maybe (NonEmpty a) -> Maybe (NonEmpty a)
concatMNonEmpty [] ne = ne
concatMNonEmpty (x : xs) Nothing = Just $ x :| xs
concatMNonEmpty (x : xs) (Just (y :| ys)) = Just $ x :| xs <> (y : ys)

-- | Prepends a value to the non-empty, turning it into a Just.
--
-- @since 0.1
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

-- | Transforms a foldable's unique elements into a 'Seq'.
--
-- @since 0.1
uniqueSeqFromList :: forall f a. (Foldable f, Hashable a) => f a -> Seq a
uniqueSeqFromList = view _2 . foldr go ((∅), (∅))
  where
    go :: a -> (HashSet a, Seq a) -> (HashSet a, Seq a)
    go x (found, acc)
      | x ∉ found = (x ⟇ found, acc ⋗ x)
      | otherwise = (found, acc)
