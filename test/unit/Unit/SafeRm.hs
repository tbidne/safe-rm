-- | Property tests for the SafeRm API.
--
-- @since 0.1
module Unit.SafeRm
  ( tests,
  )
where

import Data.Char qualified as Ch
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.List qualified as L
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import SafeRm qualified
import SafeRm.Data.Index (Index (unIndex))
import SafeRm.Data.Metadata (Metadata (numEntries, numFiles))
import SafeRm.Data.PathData (PathData (originalPath))
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    _MkPathI,
  )
import SafeRm.Exceptions (ExceptionI (MkExceptionI), ExceptionIndex (SomeExceptions))
import Unit.MaxRuns (MaxRuns (MkMaxRuns))
import Unit.Prelude

tests :: IO FilePath -> TestTree
tests testDir =
  testGroup
    "SafeRm"
    [ delete testDir,
      deleteSome testDir,
      deletePermanently testDir,
      deleteSomePermanently testDir,
      restore testDir,
      restoreSome testDir,
      empty testDir,
      metadata testDir
    ]

delete :: IO FilePath -> TestTree
delete mtestDir = askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "All paths are deleted" "delete" $ do
    withTests limit $
      property $ do
        testDir <- (</> "d1") <$> liftIO mtestDir
        α <- forAll genFileNameSet
        let αTest = φ (testDir </>) α
            trashDir = testDir </> ".trash"
            αTrash = φ (trashDir </>) α
            mtrashHome = Just $ MkPathI trashDir

        annotateShow αTest

        -- create files and assert existence
        liftIO $ do
          clearDirectory testDir
          createFilesMap φ αTest
        assertFilesExist αTest

        -- delete files
        liftIO $ SafeRm.delete False mtrashHome (φ MkPathI αTest)

        -- assert original files moved to trash
        assertFilesExist αTrash
        assertFilesDoNotExist αTest

        -- get index
        index <- view #unIndex <$> SafeRm.getIndex mtrashHome
        annotateShow index

        let indexOrigPaths = Map.foldl' toOrigPath (∅) index

        αTest === indexOrigPaths

deleteSome :: IO FilePath -> TestTree
deleteSome mtestDir = askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "Some paths are deleted, others error" "deleteSome" $ do
    withTests limit $
      property $ do
        testDir <- (</> "d2") <$> liftIO mtestDir
        (α, β) <- forAll gen2FileNameSets
        let toTestDir = φ (testDir </>)

            αTest = toTestDir α
            trashDir = testDir </> ".trash"
            mtrashHome = Just $ MkPathI trashDir

        annotateShow αTest

        -- create files and assert existence
        liftIO $ do
          clearDirectory testDir
          createFilesMap φ αTest
        assertFilesExist αTest

        -- delete files
        -- should succeed on α and fail on β
        let toDelete = αTest ∪ toTestDir β
        caughtEx <-
          liftIO $
            (SafeRm.delete False mtrashHome (φ MkPathI toDelete) $> Nothing)
              `catch` \(ex :: ExceptionI SomeExceptions) -> do
                pure $ Just ex

        (MkExceptionI exs) <-
          maybe
            (annotate "Expected exceptions, received none" *> failure)
            pure
            caughtEx

        annotateShow exs

        -- should have received exactly 1 exception for each bad filename
        length exs === length β

        -- assert original files moved to trash
        assertFilesExist (φ (trashDir </>) α)
        assertFilesDoNotExist (φ (trashDir </>) β)

        -- get index
        index <- view #unIndex <$> SafeRm.getIndex mtrashHome
        annotateShow index

        let indexOrigPaths = Map.foldl' toOrigPath (∅) index

        -- index should exactly match α
        αTest === indexOrigPaths

deletePermanently :: IO FilePath -> TestTree
deletePermanently mtestDir = askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed desc "deletePermanently" $ do
    withTests limit $
      property $ do
        testDir <- (</> "x1") <$> liftIO mtestDir
        α <- forAll genFileNameSet
        let trashDir = testDir </> ".trash"
            αTest = φ (testDir </>) α
            αTrash = φ (trashDir </>) α
            mtrashHome = Just $ MkPathI trashDir

        annotateShow αTest

        -- create files and assert existence
        liftIO $ do
          clearDirectory testDir
          createFilesMap φ αTest
        assertFilesExist αTest

        -- delete files
        liftIO $ SafeRm.delete False mtrashHome (φ MkPathI αTest)

        -- assert original files moved to trash
        assertFilesExist αTrash
        assertFilesDoNotExist αTest

        -- permanently delete files
        let toPermDelete = φ MkPathI α
        liftIO $ SafeRm.deletePermanently False mtrashHome True toPermDelete

        -- get index
        index <- view #unIndex <$> SafeRm.getIndex mtrashHome
        annotateShow index

        (∅) === index
        assertFilesDoNotExist (αTest ∪ αTrash)
  where
    desc = "All trash entries are permanently deleted"

deleteSomePermanently :: IO FilePath -> TestTree
deleteSomePermanently mtestDir = askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed desc "deleteSomePermanently" $ do
    withTests limit $
      property $ do
        testDir <- (</> "x2") <$> liftIO mtestDir
        (α, β, γ) <- forAll gen3FileNameSets
        let toTestDir = φ (testDir </>)
            toTrashDir = φ (trashDir </>)

            toDelete = toTestDir (α ∪ γ)
            trashDir = testDir </> ".trash"
            trashSet = toTrashDir (α ∪ γ)
            mtrashHome = Just $ MkPathI trashDir

        annotateShow testDir
        annotateShow toDelete

        -- create files and assert existence
        liftIO $ do
          clearDirectory testDir
          createFilesMap φ toDelete
        assertFilesExist toDelete

        -- delete files
        liftIO $ SafeRm.delete False mtrashHome (φ MkPathI toDelete)

        -- assert original files moved to trash
        assertFilesExist trashSet
        assertFilesDoNotExist toDelete

        -- permanently delete files
        -- should succeed on α and fail on β
        let toPermDelete = φ MkPathI (α ∪ β)
        annotateShow toPermDelete
        caughtEx <-
          liftIO $
            (SafeRm.deletePermanently False mtrashHome True toPermDelete $> Nothing)
              `catch` \(ex :: ExceptionI SomeExceptions) -> do
                pure $ Just ex

        (MkExceptionI exs) <-
          maybe
            (annotate "Expected exceptions, received none" *> failure)
            pure
            caughtEx

        annotateShow exs

        -- get index
        index <- view #unIndex <$> SafeRm.getIndex mtrashHome
        annotateShow index
        let indexOrigPaths = Map.foldl' toOrigPath (∅) index

        -- γ should still exist in the trash index
        toTestDir γ === indexOrigPaths

        assertFilesDoNotExist (φ (trashDir </>) (α ∪ β))
        assertFilesExist (φ (trashDir </>) γ)
  where
    desc = "Some trash entries are permanently deleted, others error"

restore :: IO FilePath -> TestTree
restore mtestDir = askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "Restores all trash entries" "restore" $ do
    withTests limit $
      property $ do
        testDir <- (</> "r1") <$> liftIO mtestDir
        α <- forAll genFileNameSet
        let αTest = φ (testDir </>) α
            trashDir = testDir </> ".trash"
            αTrash = φ (trashDir </>) α
            mtrashHome = Just $ MkPathI trashDir

        annotateShow αTest

        -- create files and assert existence
        liftIO $ do
          clearDirectory testDir
          createFilesMap φ αTest
        assertFilesExist αTest

        -- delete files
        liftIO $ SafeRm.delete False mtrashHome (φ MkPathI αTest)

        -- assert original files moved to trash
        assertFilesExist αTrash
        assertFilesDoNotExist αTest

        -- restore files
        let toRestore = φ MkPathI α
        liftIO $ SafeRm.restore False mtrashHome toRestore

        -- get index
        index <- view #unIndex <$> SafeRm.getIndex mtrashHome
        annotateShow index

        (∅) === index
        assertFilesExist αTest
        assertFilesDoNotExist αTrash

restoreSome :: IO FilePath -> TestTree
restoreSome mtestDir = askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed desc "restoreSome" $ do
    withTests limit $
      property $ do
        testDir <- (</> "r2") <$> liftIO mtestDir
        (α, β, γ) <- forAll gen3FileNameSets
        let toTestDir = φ (testDir </>)
            toTrashDir = φ (trashDir </>)

            toDelete = toTestDir (α ∪ γ)
            trashDir = testDir </> ".trash"
            trashSet = toTrashDir α
            mtrashHome = Just $ MkPathI trashDir

        annotateShow testDir
        annotateShow toDelete

        -- create files and assert existence
        liftIO $ do
          clearDirectory testDir
          createFilesMap φ toDelete
        assertFilesExist toDelete

        -- delete files
        liftIO $ SafeRm.delete False mtrashHome (φ MkPathI toDelete)

        -- assert original files moved to trash
        assertFilesExist trashSet
        assertFilesDoNotExist toDelete

        -- restore
        -- should succeed on α and fail on β
        let toRestore = φ MkPathI (α ∪ β)
        annotateShow toRestore
        caughtEx <-
          liftIO $
            (SafeRm.restore False mtrashHome toRestore $> Nothing)
              `catch` \(ex :: ExceptionI SomeExceptions) -> do
                pure $ Just ex

        (MkExceptionI exs) <-
          maybe
            (annotate "Expected exceptions, received none" *> failure)
            pure
            caughtEx

        annotateShow exs

        -- get index
        index <- view #unIndex <$> SafeRm.getIndex mtrashHome
        annotateShow index
        let indexOrigPaths = Map.foldl' toOrigPath (∅) index

        -- γ should still exist in the trash index
        toTestDir γ === indexOrigPaths

        assertFilesExist (toTestDir α ∪ toTrashDir γ)
        assertFilesDoNotExist (toTrashDir α)
  where
    desc = "Some trash entries are restored, others error"

empty :: IO FilePath -> TestTree
empty mtestDir = askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "Empties the trash" "empty" $ do
    withTests limit $
      property $ do
        testDir <- (</> "e1") <$> liftIO mtestDir
        α <- forAll genFileNameSet
        let aTest = φ (testDir </>) α
            trashDir = testDir </> ".trash"
            aTrash = φ (trashDir </>) α
            mtrashHome = Just $ MkPathI trashDir

        annotateShow testDir
        annotateShow aTest

        -- create files and assert existence
        liftIO $ do
          clearDirectory testDir
          createFilesMap φ aTest
        assertFilesExist aTest

        -- delete files
        liftIO $ SafeRm.delete False mtrashHome (φ MkPathI aTest)

        -- assert original files moved to trash
        assertFilesExist aTrash
        assertFilesDoNotExist aTest

        -- empty trash
        liftIO $ SafeRm.empty mtrashHome

        -- get index
        index <- view #unIndex <$> SafeRm.getIndex mtrashHome
        annotateShow index

        (∅) === index
        assertFilesDoNotExist aTest
        assertFilesDoNotExist aTrash

metadata :: IO FilePath -> TestTree
metadata mtestDir = askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "Retrieves metadata" "metadata" $ do
    withTests limit $
      property $ do
        testDir <- (</> "m1") <$> liftIO mtestDir
        α <- forAll genFileNameSet
        let aTest = φ (testDir </>) α
            trashDir = testDir </> ".trash"
            aTrash = φ (trashDir </>) α
            mtrashHome = Just $ MkPathI trashDir

        annotateShow testDir
        annotateShow aTest

        -- create files and assert existence
        liftIO $ do
          clearDirectory testDir
          createFilesMap φ aTest
        assertFilesExist aTest

        -- delete files
        liftIO $ SafeRm.delete False mtrashHome (φ MkPathI aTest)

        -- assert original files moved to trash
        assertFilesExist aTrash
        assertFilesDoNotExist aTest

        -- empty trash
        metadata' <- liftIO $ SafeRm.getMetadata mtrashHome

        length α === natToInt (metadata' ^. #numEntries)
        length α === natToInt (metadata' ^. #numFiles)

natToInt :: HasCallStack => Natural -> Int
natToInt i
  | i <= intMax = fromIntegral i
  | otherwise = error $ "natToInt: Too large to convert to Int: " <> show i
  where
    intMax = fromIntegral (maxBound :: Int)

genFileNameSet :: Gen (HashSet FilePath)
genFileNameSet = Set.fromList <$> Gen.list range genFileName
  where
    range = Range.linear 0 100

gen2FileNameSets :: Gen (HashSet FilePath, HashSet FilePath)
gen2FileNameSets = do
  α <- Set.fromList <$> Gen.list range genFileName
  β <- Set.fromList <$> Gen.list range (genFileNameNoDupes α)
  pure (α, β)
  where
    range = Range.linear 1 100

gen3FileNameSets :: Gen (HashSet FilePath, HashSet FilePath, HashSet FilePath)
gen3FileNameSets = do
  α <- Set.fromList <$> Gen.list range genFileName
  β <- Set.fromList <$> Gen.list range (genFileNameNoDupes α)
  γ <- Set.fromList <$> Gen.list range (genFileNameNoDupes (α ∪ β))
  pure (α, β, γ)
  where
    range = Range.linear 1 100

genFileName :: Gen FilePath
genFileName = genFileNameNoDupes (∅)

genFileNameNoDupes :: HashSet FilePath -> Gen FilePath
genFileNameNoDupes paths =
  Gen.filter
    (∉ paths)
    (Gen.string range genChar)
  where
    range = Range.linear 1 20

genChar :: Gen Char
genChar = Gen.filterT (not . badChars) Gen.unicode
  where
    badChars c = Ch.isControl c || L.elem c ['/', '.']

toOrigPath :: HashSet FilePath -> PathData -> HashSet FilePath
toOrigPath acc pd = Set.insert (pd ^. #originalPath % _MkPathI) acc

(∪) :: Hashable a => HashSet a -> HashSet a -> HashSet a
(∪) = Set.union

infixl 6 ∪

(∈) :: Hashable a => a -> HashSet a -> Bool
(∈) = Set.member

infix 4 ∈

(∉) :: Hashable a => a -> HashSet a -> Bool
(∉) x = not . (∈) x

infix 4 ∉

φ :: Hashable b => (a -> b) -> HashSet a -> HashSet b
φ = Set.map

class Empty a where
  (∅) :: a

instance Empty (HashSet a) where
  (∅) = Set.empty

instance Empty (HashMap k v) where
  (∅) = Map.empty
