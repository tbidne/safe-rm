{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'UniqueSeq' type.
--
-- @since 0.1
module SafeRm.Data.UniqueSeq
  ( UniqueSeq (MkUniqueSeq),
    fromFoldable,
    fromSet,
  )
where

import Data.HashSet qualified as Set
import GHC.Exts (IsList (Item, fromList, toList))
import Optics.Core (A_Getter, LabelOptic (labelOptic), to)
import SafeRm.Prelude

-- | Like 'Seq' but with the guarantee that all elements are unique.
--
-- @since 0.1
data UniqueSeq a = UnsafeUniqueSeq
  { seq :: !(Seq a),
    set :: !(HashSet a)
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Foldable UniqueSeq where
  foldr f x (UnsafeUniqueSeq seq _) = foldr f x seq

-- | @since 0.1
pattern MkUniqueSeq :: Seq a -> HashSet a -> UniqueSeq a
pattern MkUniqueSeq seq set <- UnsafeUniqueSeq seq set

{-# COMPLETE MkUniqueSeq #-}

-- | @since 0.1
instance
  (k ~ A_Getter, b ~ Seq a, c ~ Seq a) =>
  LabelOptic "seq" k (UniqueSeq a) (UniqueSeq a) b c
  where
  labelOptic = to (\(UnsafeUniqueSeq seq _) -> seq)

-- | @since 0.1
instance
  (k ~ A_Getter, b ~ HashSet a, c ~ HashSet a) =>
  LabelOptic "set" k (UniqueSeq a) (UniqueSeq a) b c
  where
  labelOptic = to (\(UnsafeUniqueSeq _ set) -> set)

-- | @since 0.1
instance Hashable a => Semigroup (UniqueSeq a) where
  (<>) = (∪)

-- | @since 0.1
instance Hashable a => Monoid (UniqueSeq a) where
  mempty = (∅)

-- | @since 0.1
instance Empty (UniqueSeq a) where
  (∅) = UnsafeUniqueSeq (∅) (∅)

-- | @since 0.1
instance Hashable a => Union (UniqueSeq a) where
  UnsafeUniqueSeq xseq xset ∪ UnsafeUniqueSeq yseq yset =
    UnsafeUniqueSeq (view _2 (foldr go ((∅), (∅)) (xseq <> yseq))) (xset ∪ yset)
    where
      go z (found, acc)
        | z ∉ found = (Set.insert z found, acc |> z)
        | otherwise = (found, acc)

-- | @since 0.1
instance Hashable a => Member (UniqueSeq a) where
  type MKey (UniqueSeq a) = a
  x ∈ UnsafeUniqueSeq _ set = x ∈ set

-- | @since 0.1
instance Hashable a => Sequenced (UniqueSeq a) where
  type SElem (UniqueSeq a) = a
  (⋗) = flip (insert (flip (|>)))
  (⋖) = insert (<|)

-- | @since 0.1
instance CMap UniqueSeq where
  type CMapC UniqueSeq a = Hashable a
  φ f (UnsafeUniqueSeq seq set) = UnsafeUniqueSeq (φ f seq) (φ f set)

-- | @since 0.1
instance Hashable a => IsList (UniqueSeq a) where
  type Item (UniqueSeq a) = a
  toList (UnsafeUniqueSeq seq _) = toList seq

  -- TODO: should verify that order is maintained
  fromList = fromFoldable

-- | @since 0.1
insert :: Hashable a => (a -> Seq a -> Seq a) -> a -> UniqueSeq a -> UniqueSeq a
insert seqIns x useq@(UnsafeUniqueSeq seq set)
  | x ∉ useq = UnsafeUniqueSeq (seqIns x seq) (x ⟇ set)
  | otherwise = useq

fromFoldable :: (Foldable f, Hashable a) => f a -> UniqueSeq a
fromFoldable = foldr (flip (⋗)) (∅)

fromSet :: HashSet a -> UniqueSeq a
fromSet set = UnsafeUniqueSeq seq set
  where
    seq = foldr (flip (⋗)) (∅) set
