-- | Provides internal utility functions
--
-- @since 0.1
module SafeRm.Utils
  ( whenJust,
    concatMNonEmpty,
    prependMNonEmpty,
    allM1,
    formatBytes,
    normalizedFormat,
  )
where

import Data.Bytes qualified as Bytes
import Data.Bytes.Class.Wrapper (Unwrapper (Unwrapped))
import Data.Bytes.Formatting (FloatingFormatter (MkFloatingFormatter))
import Data.Bytes.Formatting.Base (BaseFormatter)
import Data.Bytes.Size (Sized)
import SafeRm.Prelude
import Text.Printf (PrintfArg)

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

-- | Normalizes and formats the bytes.
--
-- @since 0.1
normalizedFormat :: Bytes B Natural -> Text
normalizedFormat =
  formatBytes
    . Bytes.normalize
    . toDouble
  where
    toDouble :: Bytes s Natural -> Bytes s Double
    toDouble = fmap fromIntegral

-- | Formats the bytes.
--
-- @since 0.1
formatBytes ::
  ( BaseFormatter (Unwrapped a) ~ FloatingFormatter,
    PrintfArg (Unwrapped a),
    Sized a,
    Unwrapper a
  ) =>
  a ->
  Text
formatBytes =
  Bytes.formatSized
    (MkFloatingFormatter (Just 2))
    Bytes.sizedFormatterUnix
