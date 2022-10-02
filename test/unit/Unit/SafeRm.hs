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
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import SafeRm qualified
import SafeRm.Data.Index (Index (unIndex))
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
      empty testDir
    ]

delete :: IO FilePath -> TestTree
delete mtestDir = askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "All paths are deleted" "delete" $ do
    withTests limit $
      property $ do
        testDir <- (</> "d1") <$> liftIO mtestDir
        fileNames <- forAll genFileNameSet
        let pathsSet = Set.map (testDir </>) fileNames
            trashDir = testDir </> ".trash"
            trashSet = Set.map (trashDir </>) fileNames
            mtrashHome = Just $ MkPathI trashDir

        annotateShow pathsSet
        annotateShow trashSet

        -- create files and assert existence
        liftIO $ do
          clearDirectory testDir
          createFilesMap Set.map pathsSet
        assertFilesExist pathsSet

        -- delete files
        liftIO $ SafeRm.delete False mtrashHome (Set.map MkPathI pathsSet)

        -- assert original files moved to trash
        assertFilesExist trashSet
        assertFilesDoNotExist pathsSet

        -- get index
        index <- view #unIndex <$> SafeRm.getIndex mtrashHome
        annotateShow index

        let indexOrigPaths = Map.foldl' toOrigPath Set.empty index

        pathsSet === indexOrigPaths

deleteSome :: IO FilePath -> TestTree
deleteSome mtestDir = askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "Some paths are deleted, others error" "deleteSome" $ do
    withTests limit $
      property $ do
        testDir <- (</> "d2") <$> liftIO mtestDir
        (goodFileNames, badFileNames) <- forAll gen2FileNameSets
        let goodPathsSet = Set.map (testDir </>) goodFileNames
            badPathsSet = Set.map (testDir </>) badFileNames
            allPathsSet = Set.union goodPathsSet badPathsSet
            trashDir = testDir </> ".trash"
            mtrashHome = Just $ MkPathI trashDir

        annotateShow goodPathsSet

        -- create files and assert existence
        liftIO $ do
          clearDirectory testDir
          createFilesMap Set.map goodPathsSet
        assertFilesExist goodPathsSet

        -- delete files
        caughtEx <-
          liftIO $
            (SafeRm.delete False mtrashHome (Set.map MkPathI allPathsSet) $> Nothing)
              `catch` \(ex :: ExceptionI SomeExceptions) -> do
                pure $ Just ex

        (MkExceptionI exs) <-
          maybe
            (annotate "Expected exceptions, received none" *> failure)
            pure
            caughtEx

        annotateShow exs

        -- should have received exactly 1 exception for each bad filename
        length exs === length badFileNames

        -- assert original files moved to trash
        assertFilesExist (Set.map (trashDir </>) goodFileNames)
        assertFilesDoNotExist (Set.map (trashDir </>) badFileNames)

        -- get index
        index <- view #unIndex <$> SafeRm.getIndex mtrashHome
        annotateShow index

        let indexOrigPaths = Map.foldl' toOrigPath Set.empty index

        -- index should have exactly the good paths
        goodPathsSet === indexOrigPaths

deletePermanently :: IO FilePath -> TestTree
deletePermanently mtestDir = askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed desc "deletePermanently" $ do
    withTests limit $
      property $ do
        testDir <- (</> "x1") <$> liftIO mtestDir
        fileNames <- forAll genFileNameSet
        let pathsSet = Set.map (testDir </>) fileNames
            trashDir = testDir </> ".trash"
            trashSet = Set.map (trashDir </>) fileNames
            mtrashHome = Just $ MkPathI trashDir

        annotateShow pathsSet
        annotateShow trashSet

        -- create files and assert existence
        liftIO $ do
          clearDirectory testDir
          createFilesMap Set.map pathsSet
        assertFilesExist pathsSet

        -- delete files
        liftIO $ SafeRm.delete False mtrashHome (Set.map MkPathI pathsSet)

        -- assert original files moved to trash
        assertFilesExist trashSet
        assertFilesDoNotExist pathsSet

        -- permanently delete files
        let fileNamesToDelete = Set.map MkPathI fileNames
        liftIO $ SafeRm.deletePermanently False mtrashHome True fileNamesToDelete

        -- get index
        index <- view #unIndex <$> SafeRm.getIndex mtrashHome
        annotateShow index

        assert $ Map.null index
        assertFilesDoNotExist pathsSet
        assertFilesDoNotExist trashSet
  where
    desc = "All trash entries are permanently deleted"

deleteSomePermanently :: IO FilePath -> TestTree
deleteSomePermanently mtestDir = askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed desc "deleteSomePermanently" $ do
    withTests limit $
      property $ do
        testDir <- (</> "x2") <$> liftIO mtestDir
        (goodFileNames, badFileNames) <- forAll gen2FileNameSets
        let goodPathsSet = Set.map (testDir </>) goodFileNames
            -- good + bad trash paths
            allFileNames = Set.union goodFileNames badFileNames
            trashDir = testDir </> ".trash"
            trashSet = Set.map (trashDir </>) goodFileNames
            mtrashHome = Just $ MkPathI trashDir

        annotateShow goodPathsSet

        -- create files and assert existence
        liftIO $ do
          clearDirectory testDir
          createFilesMap Set.map goodPathsSet
        assertFilesExist goodPathsSet

        -- delete files
        liftIO $ SafeRm.delete False mtrashHome (Set.map MkPathI goodPathsSet)

        -- assert original files moved to trash
        assertFilesExist trashSet
        assertFilesDoNotExist goodPathsSet

        -- permanently delete files
        let fileNamesToDelete = Set.map MkPathI allFileNames
        annotateShow fileNamesToDelete
        caughtEx <-
          liftIO $
            (SafeRm.deletePermanently False mtrashHome True fileNamesToDelete $> Nothing)
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

        assert $ Map.null index
        assertFilesDoNotExist goodPathsSet
        assertFilesDoNotExist (Set.map (trashDir </>) goodFileNames)
  where
    desc = "Some trash entries are permanently deleted, others error"

restore :: IO FilePath -> TestTree
restore mtestDir = askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "Restores all trash entries" "restore" $ do
    withTests limit $
      property $ do
        testDir <- (</> "r1") <$> liftIO mtestDir
        fileNames <- forAll genFileNameSet
        let pathsSet = Set.map (testDir </>) fileNames
            trashDir = testDir </> ".trash"
            trashSet = Set.map (trashDir </>) fileNames
            mtrashHome = Just $ MkPathI trashDir

        annotateShow pathsSet
        annotateShow trashSet

        -- create files and assert existence
        liftIO $ do
          clearDirectory testDir
          createFilesMap Set.map pathsSet
        assertFilesExist pathsSet

        -- delete files
        liftIO $ SafeRm.delete False mtrashHome (Set.map MkPathI pathsSet)

        -- assert original files moved to trash
        assertFilesExist trashSet
        assertFilesDoNotExist pathsSet

        -- restore files
        let fileNamesToRestore = Set.map MkPathI fileNames
        liftIO $ SafeRm.restore False mtrashHome fileNamesToRestore

        -- get index
        index <- view #unIndex <$> SafeRm.getIndex mtrashHome
        annotateShow index

        assert $ Map.null index
        assertFilesExist pathsSet
        assertFilesDoNotExist trashSet

restoreSome :: IO FilePath -> TestTree
restoreSome mtestDir = askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed desc "restoreSome" $ do
    withTests limit $
      property $ do
        testDir <- (</> "r2") <$> liftIO mtestDir
        (goodFileNames, badFileNames) <- forAll gen2FileNameSets
        let goodPathsSet = Set.map (testDir </>) goodFileNames
            -- good + bad trash paths
            allFileNames = Set.union goodFileNames badFileNames
            trashDir = testDir </> ".trash"
            trashSet = Set.map (trashDir </>) goodFileNames
            mtrashHome = Just $ MkPathI trashDir

        annotateShow goodPathsSet

        -- create files and assert existence
        liftIO $ do
          clearDirectory testDir
          createFilesMap Set.map goodPathsSet
        assertFilesExist goodPathsSet

        -- delete files
        liftIO $ SafeRm.delete False mtrashHome (Set.map MkPathI goodPathsSet)

        -- assert original files moved to trash
        assertFilesExist trashSet
        assertFilesDoNotExist goodPathsSet

        -- restore
        let fileNamesToRestore = Set.map MkPathI allFileNames
        annotateShow fileNamesToRestore
        caughtEx <-
          liftIO $
            (SafeRm.restore False mtrashHome fileNamesToRestore $> Nothing)
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

        assert $ Map.null index
        assertFilesExist goodPathsSet
        assertFilesDoNotExist (Set.map (trashDir </>) goodFileNames)
  where
    desc = "Some trash entries are restored, others error"

empty :: IO FilePath -> TestTree
empty mtestDir = askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "Empties the trash" "empty" $ do
    withTests limit $
      property $ do
        testDir <- (</> "e1") <$> liftIO mtestDir
        fileNames <- forAll genFileNameSet
        let pathsSet = Set.map (testDir </>) fileNames
            trashDir = testDir </> ".trash"
            trashSet = Set.map (trashDir </>) fileNames
            mtrashHome = Just $ MkPathI trashDir

        annotateShow pathsSet
        annotateShow trashSet

        -- create files and assert existence
        liftIO $ do
          clearDirectory testDir
          createFilesMap Set.map pathsSet
        assertFilesExist pathsSet

        -- delete files
        liftIO $ SafeRm.delete False mtrashHome (Set.map MkPathI pathsSet)

        -- assert original files moved to trash
        assertFilesExist trashSet
        assertFilesDoNotExist pathsSet

        -- empty trash
        liftIO $ SafeRm.empty mtrashHome

        -- get index
        index <- view #unIndex <$> SafeRm.getIndex mtrashHome
        annotateShow index

        assert $ Map.null index
        assertFilesDoNotExist pathsSet
        assertFilesDoNotExist trashSet

genFileNameSet :: Gen (HashSet FilePath)
genFileNameSet = Set.fromList <$> Gen.list range genFileName
  where
    range = Range.linear 0 100

gen2FileNameSets :: Gen (HashSet FilePath, HashSet FilePath)
gen2FileNameSets = do
  l <- Set.fromList <$> Gen.list range genFileName
  r <- Set.fromList <$> Gen.list range (genFileNameNoDupes l)
  pure (l, r)
  where
    range = Range.linear 1 100

genFileName :: Gen FilePath
genFileName = Gen.string range genChars
  where
    range = Range.linear 1 20
    genChars = Gen.filterT (not . badChars) Gen.unicode
    badChars c = Ch.isControl c || c == '/'

genFileNameNoDupes :: HashSet FilePath -> Gen FilePath
genFileNameNoDupes paths =
  Gen.filter
    (not . flip Set.member paths)
    (Gen.string range genChars)
  where
    range = Range.linear 1 20
    genChars = Gen.filterT (not . badChars) Gen.unicode
    badChars c = Ch.isControl c || c == '/'

toOrigPath :: HashSet FilePath -> PathData -> HashSet FilePath
toOrigPath acc pd = Set.insert (pd ^. #originalPath % _MkPathI) acc
