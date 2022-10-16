module Unit.Data.UniqueSeq
  ( tests,
  )
where

import Data.Sequence (Seq (Empty, (:<|)))
import GHC.Exts (IsList (fromList, toList))
import Hedgehog (PropertyT)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import SafeRm.Data.UniqueSeq (UniqueSeq (MkUniqueSeq))
import SafeRm.Data.UniqueSeq qualified as UniqueSet
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Data.UniqueSeq"
    [ invariantTests,
      lawsTests
    ]

invariantTests :: TestTree
invariantTests =
  testGroup
    "General Invariants"
    [ isListIsomorphism,
      isListOrder,
      fromFoldableOrder,
      cmapInvariant,
      insertInvariant
    ]

isListIsomorphism :: TestTree
isListIsomorphism = askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "fromList . toList === id" "isListIsomorphism" $ do
    withTests limit $
      property $ do
        xs <- forAll genUniqueSeq
        let useq = toList xs
        annotateShow useq
        xs === fromList useq

isListOrder :: TestTree
isListOrder = askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "toList . fromList preserves order" "isListOrder" $ do
    withTests limit $
      property $ do
        origList <- forAll genUniqueList
        let useq = fromList @(UniqueSeq Int) origList
        annotateShow useq

        let newList = toList useq
        annotateShow newList

        compareLists (∅) newList origList
  where
    -- For newList, origList, want to verify that the lists are the same,
    -- modulo newList omitting duplicates. Note that this does not check
    -- other invariants e.g. that newList does not contain duplicates.
    compareLists :: HashSet Int -> [Int] -> [Int] -> PropertyT IO ()
    -- Base case 1: both lists are empty -> equal
    compareLists _ [] [] = pure ()
    -- Base case 2: newList is non-empty but orig list is empty -> failure
    compareLists _ new@(_ : _) [] = do
      annotate "New list has more elements than original"
      annotateShow new
      failure
    -- Base case 3: newList is empty but original is not -> okay as long
    -- as every element left is a duplicate.
    compareLists found [] orig =
      for_ orig $ \i -> do
        annotateShow orig
        annotateShow found
        annotateShow i
        assert (i ∈ found)
    -- Inductive case
    compareLists found (n : ns) (o : os)
      -- n and o are equal -> okay
      | n == o = compareLists found' ns os
      -- Not equal -> y _should_ be a duplicate, so verify and skip all
      -- other dupes before continuing.
      | otherwise = do
          if o ∈ found
            then -- Original value o is a duplicate -> okay. Try again on the
            -- rest of the original list.
              compareLists found (n : ns) os
            else -- Original value o is not a duplicate -> failure.
            do
              annotate "Non-duplicate missing from new list"
              annotateShow o
              failure
      where
        found' = n ⟇ found

fromFoldableOrder :: TestTree
fromFoldableOrder = askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "fromFoldable preserves order" "fromFoldableOrder" $ do
    withTests limit $
      property $ do
        xs <- forAll genUniqueList
        let useq@(MkUniqueSeq seq _) = UniqueSet.fromFoldable xs

        annotateShow seq
        sameOrder xs seq

        uniqseqInvariants useq
  where
    sameOrder [] Empty = pure ()
    sameOrder (x : _) Empty = annotateShow x *> failure
    sameOrder [] (y :<| _) = annotateShow y *> failure
    sameOrder (x : xs) (y :<| ys) = do
      x === y
      sameOrder xs ys

cmapInvariant :: TestTree
cmapInvariant = askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "cmap invariants" "cmapInvariant" $ do
    withTests limit $
      property $ do
        useq <- forAll genUniqueSeq
        let useq' = φ even useq
        uniqseqInvariants useq'

insertInvariant :: TestTree
insertInvariant = askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "insert invariants" "insertInvariant" $ do
    withTests limit $
      property $ do
        xs <- forAll genList
        let useqPrepend = foldr (⋖) (∅) xs
            useqAppend = foldl' (⋗) (∅) xs

        uniqseqInvariants useqPrepend
        uniqseqInvariants useqAppend

lawsTests :: TestTree
lawsTests =
  testGroup
    "Laws"
    [ monoid,
      insertMember
    ]

monoid :: TestTree
monoid = askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "union is a monoid" "unionMonoid" $ do
    withTests limit $
      property $ do
        α <- forAll genUniqueSeq
        β <- forAll genUniqueSeq
        γ <- forAll genUniqueSeq

        annotate "Identity"
        α === α ∪ (∅)
        α === (∅) ∪ α

        annotate "Associativity"
        (α ∪ β) ∪ γ === α ∪ (β ∪ γ)

insertMember :: TestTree
insertMember = askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "x ∈ insert x useq" "insertMember" $ do
    withTests limit $
      property $ do
        useq <- forAll genUniqueSeq
        x <- forAll genInt

        assert $ x ∈ useq ⋗ x
        assert $ x ∈ x ⋖ useq

uniqseqInvariants :: (Hashable a, Show a) => UniqueSeq a -> PropertyT IO ()
uniqseqInvariants useq = do
  foundRef <- newIORef (∅)
  seqAndSetSynced useq
  seqUnique foundRef useq

seqAndSetSynced :: (Hashable a, Show a) => UniqueSeq a -> PropertyT IO ()
seqAndSetSynced (MkUniqueSeq seq set) = do
  annotateShow seq
  annotateShow set
  -- same size
  (♯) seq === (♯) set

  -- all seq in set
  for_ seq $ \x -> do
    annotateShow x
    assert $ x ∈ seq

seqUnique ::
  forall a.
  (Hashable a, Show a) =>
  IORef (HashSet a) ->
  UniqueSeq a ->
  PropertyT IO ()
seqUnique foundRef (MkUniqueSeq seq _) = foldr go (pure ()) seq
  where
    go :: a -> PropertyT IO () -> PropertyT IO ()
    go x acc = do
      found <- readIORef foundRef
      if x ∈ found
        then do
          annotate "Found duplicate"
          annotateShow x
          failure
        else do
          modifyIORef' foundRef (x ⟇)
          acc

genUniqueSeq :: Gen (UniqueSeq Int)
genUniqueSeq = fromList <$> genList

genUniqueList :: Gen [Int]
genUniqueList = do
  xs <- genList
  let (_, uniq) = foldl' go ((∅), (∅)) xs
  pure uniq
  where
    go :: (HashSet Int, [Int]) -> Int -> (HashSet Int, [Int])
    go (found, acc) y
      | y ∉ found = (y ⟇ found, y ⋖ acc)
      | otherwise = (found, acc)

genList :: Gen [Int]
genList = Gen.list listRange genInt
  where
    listRange = Range.exponential 1 10_000

genInt :: Gen Int
genInt = Gen.integral intRange
  where
    intRange = Range.linearFrom 0 minBound maxBound
