-- | Property tests for the SafeRm API.
--
-- @since 0.1
module Integration.SafeRm
  ( tests,
  )
where

import Data.Char qualified as Ch
import Data.HashMap.Strict qualified as Map
import Data.List qualified as L
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Integration.Prelude
import SafeRm qualified
import SafeRm.Data.PathData (PathData)
import SafeRm.Data.Paths (PathI (MkPathI))
import SafeRm.Data.UniqueSeq (UniqueSeq, fromFoldable)
import SafeRm.Exceptions
  ( ExceptionI (MkExceptionI),
    ExceptionIndex (SomeExceptions),
  )
import SafeRm.Runner.Env
  ( Env (MkEnv),
    LogEnv (MkLogEnv),
    logEnv,
    trashHome,
  )
import SafeRm.Runner.SafeRmT (usingSafeRmT)

-- TODO: Right now we are using runner's SafeRmT to run the tests. This works
-- fine and means we do not have to use our own custom type. But it also
-- means we cannot test the logging, and the may be something we want to do
-- at some point. Consider if we want to do this.

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
      emptyTrash testDir,
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
        env <- liftIO $ mkEnv trashDir

        annotateShow αTest

        -- create files and assert existence
        liftIO $ do
          clearDirectory testDir
          createFilesMap φ αTest
        assertFilesExist αTest

        -- delete files
        liftIO $ usingSafeRmT env $ SafeRm.delete (φ MkPathI αTest)

        -- assert original files moved to trash
        annotate "Assert files exist"
        assertFilesExist αTrash
        annotate "Assert files do not exist"
        assertFilesDoNotExist αTest

        -- get index
        index <- liftIO $ view #unIndex <$> usingSafeRmT env SafeRm.getIndex
        annotateShow index

        let indexOrigPaths = Map.foldl' toOrigPath (∅) index

        αTest ^. #set === indexOrigPaths

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
        env <- liftIO $ mkEnv trashDir

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
            (usingSafeRmT env (SafeRm.delete (φ MkPathI toDelete)) $> Nothing)
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
        annotate "Assert files exist"
        assertFilesExist (φ (trashDir </>) α)
        annotate "Assert files do not exist"
        assertFilesDoNotExist (φ (trashDir </>) β)

        -- get index
        index <- liftIO $ view #unIndex <$> usingSafeRmT env SafeRm.getIndex
        annotateShow index

        let indexOrigPaths = Map.foldl' toOrigPath (∅) index

        -- index should exactly match α
        αTest ^. #set === indexOrigPaths

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
        env <- liftIO $ mkEnv trashDir

        annotateShow αTest

        -- create files and assert existence
        liftIO $ do
          clearDirectory testDir
          createFilesMap φ αTest
        assertFilesExist αTest

        -- delete files
        liftIO $ usingSafeRmT env $ SafeRm.delete (φ MkPathI αTest)

        -- assert original files moved to trash
        annotate "Assert files exist"
        assertFilesExist αTrash
        annotate "Assert files do not exist"
        assertFilesDoNotExist αTest

        -- permanently delete files
        let toPermDelete = φ MkPathI α
        liftIO $ usingSafeRmT env $ SafeRm.deletePermanently True toPermDelete

        -- get index
        index <- liftIO $ view #unIndex <$> usingSafeRmT env SafeRm.getIndex
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
        env <- liftIO $ mkEnv trashDir

        annotateShow testDir
        annotateShow toDelete

        -- create files and assert existence
        liftIO $ do
          clearDirectory testDir
          createFilesMap φ toDelete
        assertFilesExist toDelete

        -- delete files
        liftIO $ usingSafeRmT env $ SafeRm.delete (φ MkPathI toDelete)

        -- assert original files moved to trash
        annotate "Assert files exist"
        assertFilesExist trashSet
        annotate "Assert files do not exist"
        assertFilesDoNotExist toDelete

        -- permanently delete files
        -- should succeed on α and fail on β
        let toPermDelete = φ MkPathI (α ∪ β)
        annotateShow toPermDelete
        caughtEx <-
          liftIO $
            (usingSafeRmT env (SafeRm.deletePermanently True toPermDelete) $> Nothing)
              `catch` \(ex :: ExceptionI SomeExceptions) -> do
                pure $ Just ex

        (MkExceptionI exs) <-
          maybe
            (annotate "Expected exceptions, received none" *> failure)
            pure
            caughtEx

        annotateShow exs

        -- get index
        index <- liftIO $ view #unIndex <$> usingSafeRmT env SafeRm.getIndex
        annotateShow index
        let indexOrigPaths = Map.foldl' toOrigPath (∅) index

        -- γ should still exist in the trash index
        toTestDir γ ^. #set === indexOrigPaths

        annotate "Assert files do not exist"
        assertFilesDoNotExist (φ (trashDir </>) (α ∪ β))
        annotate "Assert files exist"
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
        env <- liftIO $ mkEnv trashDir

        annotateShow αTest

        -- create files and assert existence
        liftIO $ do
          clearDirectory testDir
          createFilesMap φ αTest
        assertFilesExist αTest

        -- delete files
        liftIO $ usingSafeRmT env $ SafeRm.delete (φ MkPathI αTest)

        -- assert original files moved to trash
        annotate "Assert files exist"
        assertFilesExist αTrash
        annotate "Assert files do not exist"
        assertFilesDoNotExist αTest

        -- restore files
        let toRestore = φ MkPathI α
        liftIO $ usingSafeRmT env $ SafeRm.restore toRestore

        -- get index
        index <- liftIO $ view #unIndex <$> usingSafeRmT env SafeRm.getIndex
        annotateShow index

        (∅) === index
        annotate "Assert files exist"
        assertFilesExist αTest
        annotate "Assert files do not exist"
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
        env <- liftIO $ mkEnv trashDir

        annotateShow testDir
        annotateShow toDelete

        -- create files and assert existence
        liftIO $ do
          clearDirectory testDir
          createFilesMap φ toDelete
        assertFilesExist toDelete

        -- delete files
        liftIO $ usingSafeRmT env $ SafeRm.delete (φ MkPathI toDelete)

        -- assert original files moved to trash
        annotate "Assert files exist"
        assertFilesExist trashSet
        annotate "Assert files do not exist"
        assertFilesDoNotExist toDelete

        -- restore
        -- should succeed on α and fail on β
        let toRestore = φ MkPathI (α ∪ β)
        annotateShow toRestore
        caughtEx <-
          liftIO $
            (usingSafeRmT env (SafeRm.restore toRestore) $> Nothing)
              `catch` \(ex :: ExceptionI SomeExceptions) -> do
                pure $ Just ex

        (MkExceptionI exs) <-
          maybe
            (annotate "Expected exceptions, received none" *> failure)
            pure
            caughtEx

        annotateShow exs

        -- get index
        index <- liftIO $ view #unIndex <$> usingSafeRmT env SafeRm.getIndex
        annotateShow index
        let indexOrigPaths = Map.foldl' toOrigPath (∅) index

        -- γ should still exist in the trash index
        toTestDir γ ^. #set === indexOrigPaths

        annotate "Assert files exist"
        assertFilesExist (toTestDir α ∪ toTrashDir γ)
        annotate "Assert files do not exist"
        assertFilesDoNotExist (toTrashDir α)
  where
    desc = "Some trash entries are restored, others error"

emptyTrash :: IO FilePath -> TestTree
emptyTrash mtestDir = askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "Empties the trash" "empty" $ do
    withTests limit $
      property $ do
        testDir <- (</> "e1") <$> liftIO mtestDir
        α <- forAll genFileNameSet
        let aTest = φ (testDir </>) α
            trashDir = testDir </> ".trash"
            aTrash = φ (trashDir </>) α
        env <- liftIO $ mkEnv trashDir

        annotateShow testDir
        annotateShow aTest

        -- create files and assert existence
        liftIO $ do
          clearDirectory testDir
          createFilesMap φ aTest
        assertFilesExist aTest

        -- delete files
        liftIO $ usingSafeRmT env $ SafeRm.delete (φ MkPathI aTest)

        -- assert original files moved to trash
        annotate "Assert files exist"
        assertFilesExist aTrash
        annotate "Assert files do not exist"
        assertFilesDoNotExist aTest

        -- empty trash
        liftIO $ usingSafeRmT env $ SafeRm.emptyTrash True

        -- get index
        index <- liftIO $ view #unIndex <$> usingSafeRmT env SafeRm.getIndex
        annotateShow index

        (∅) === index
        annotate "Assert files do not exist"
        assertFilesDoNotExist (aTest ∪ aTrash)

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
        env <- liftIO $ mkEnv trashDir

        annotateShow testDir
        annotateShow aTest

        -- create files and assert existence
        liftIO $ do
          clearDirectory testDir
          createFilesMap φ aTest
        assertFilesExist aTest

        -- delete files
        liftIO $ usingSafeRmT env $ SafeRm.delete (φ MkPathI aTest)

        -- assert original files moved to trash
        annotate "Assert files exist"
        assertFilesExist aTrash
        annotate "Assert files do not exist"
        assertFilesDoNotExist aTest

        -- empty trash
        metadata' <- liftIO $ usingSafeRmT env SafeRm.getMetadata

        length α === natToInt (metadata' ^. #numEntries)
        length α === natToInt (metadata' ^. #numFiles)

natToInt :: HasCallStack => Natural -> Int
natToInt i
  | i <= intMax = fromIntegral i
  | otherwise = error $ "natToInt: Too large to convert to Int: " <> show i
  where
    intMax = fromIntegral (maxBound :: Int)

genFileNameSet :: Gen (UniqueSeq FilePath)
genFileNameSet = fromFoldable <$> Gen.list range genFileName
  where
    range = Range.linear 0 100

gen2FileNameSets :: Gen (UniqueSeq FilePath, UniqueSeq FilePath)
gen2FileNameSets = do
  α <- fromFoldable <$> Gen.list range genFileName
  β <- fromFoldable <$> Gen.list range (genFileNameNoDupes α)
  pure (α, β)
  where
    range = Range.linear 1 100

gen3FileNameSets :: Gen (UniqueSeq FilePath, UniqueSeq FilePath, UniqueSeq FilePath)
gen3FileNameSets = do
  α <- fromFoldable <$> Gen.list range genFileName
  β <- fromFoldable <$> Gen.list range (genFileNameNoDupes α)
  γ <- fromFoldable <$> Gen.list range (genFileNameNoDupes (α ∪ β))
  pure (α, β, γ)
  where
    range = Range.linear 1 100

genFileName :: Gen FilePath
genFileName = genFileNameNoDupes (∅)

genFileNameNoDupes :: UniqueSeq FilePath -> Gen FilePath
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
toOrigPath acc pd = (pd ^. #originalPath % #unPathI) ⟇ acc

mkEnv :: FilePath -> IO Env
mkEnv fp = do
  pure $
    MkEnv
      { trashHome = MkPathI fp,
        logEnv = MkLogEnv Nothing (∅)
      }
