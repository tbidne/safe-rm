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
    PathIndex (OriginalPath),
    _MkPathI,
  )
import SafeRm.Data.Paths qualified as Paths
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
      restore testDir,
      empty testDir
    ]

delete :: IO FilePath -> TestTree
delete mtestDir = askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "All paths are deleted" "delete" $ do
    withTests limit $
      property $ do
        testDir <- (</> "d1") <$> liftIO mtestDir
        fileNames <- forAll genFileNameSet
        let pathSet = Set.map (Paths.liftPathI (testDir </>)) fileNames
            fileNamesList = view _MkPathI <$> Set.toList fileNames
            pathsList = (testDir </>) <$> fileNamesList
            trashDir = testDir </> ".trash"
            trashList = (trashDir </>) <$> fileNamesList
            mtrashHome = Just $ MkPathI trashDir

        annotateShow pathsList
        annotateShow trashList

        -- create files and assert existence
        liftIO $ do
          clearDirectory testDir
          createFiles pathsList
        assertFilesExist (view _MkPathI <$> Set.toList pathSet)

        -- delete files
        liftIO $ SafeRm.delete False mtrashHome pathSet

        -- assert original files moved to trash
        assertFilesExist trashList
        assertFilesDoNotExist pathsList

        -- get index
        index <- view #unIndex <$> SafeRm.getIndex mtrashHome
        annotateShow index

        let indexOrigPaths = Map.foldl' toOrigPath Set.empty index
            originalPaths = Set.map (view _MkPathI) pathSet

        originalPaths === indexOrigPaths

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
  testPropertyNamed "All trash entries are permanently deleted" "deletePermanently" $ do
    withTests limit $
      property $ do
        testDir <- (</> "x1") <$> liftIO mtestDir
        fileNames <- forAll genFileNameSet
        let pathSet = Set.map (Paths.liftPathI (testDir </>)) fileNames
            fileNamesList = view _MkPathI <$> Set.toList fileNames
            pathsList = (testDir </>) <$> fileNamesList
            trashDir = testDir </> ".trash"
            trashList = (trashDir </>) <$> fileNamesList
            mtrashHome = Just $ MkPathI trashDir

        annotateShow pathsList
        annotateShow trashList

        -- create files and assert existence
        liftIO $ do
          clearDirectory testDir
          createFiles pathsList
        assertFilesExist (view _MkPathI <$> Set.toList pathSet)

        -- delete files
        liftIO $ SafeRm.delete False mtrashHome pathSet

        -- assert original files moved to trash
        assertFilesExist trashList
        assertFilesDoNotExist pathsList

        -- permanently delete files
        let fileNamesToDelete = Set.map Paths.reindex fileNames
        liftIO $ SafeRm.deletePermanently False mtrashHome True fileNamesToDelete

        -- get index
        index <- view #unIndex <$> SafeRm.getIndex mtrashHome
        annotateShow index

        assert $ Map.null index
        assertFilesDoNotExist pathsList
        assertFilesDoNotExist trashList

restore :: IO FilePath -> TestTree
restore mtestDir = askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "Restores all trash entries" "restore" $ do
    withTests limit $
      property $ do
        testDir <- (</> "r1") <$> liftIO mtestDir
        fileNames <- forAll genFileNameSet
        let pathSet = Set.map (Paths.liftPathI (testDir </>)) fileNames
            fileNamesList = view _MkPathI <$> Set.toList fileNames
            pathsList = (testDir </>) <$> fileNamesList
            trashDir = testDir </> ".trash"
            trashList = (trashDir </>) <$> fileNamesList
            mtrashHome = Just $ MkPathI trashDir

        annotateShow pathsList
        annotateShow trashList

        -- create files and assert existence
        liftIO $ do
          clearDirectory testDir
          createFiles pathsList
        assertFilesExist (view _MkPathI <$> Set.toList pathSet)

        -- delete files
        liftIO $ SafeRm.delete False mtrashHome pathSet

        -- assert original files moved to trash
        assertFilesExist trashList
        assertFilesDoNotExist pathsList

        -- restore files
        let fileNamesToRestore = Set.map Paths.reindex fileNames
        liftIO $ SafeRm.restore False mtrashHome fileNamesToRestore

        -- get index
        index <- view #unIndex <$> SafeRm.getIndex mtrashHome
        annotateShow index

        assert $ Map.null index
        assertFilesExist pathsList
        assertFilesDoNotExist trashList

empty :: IO FilePath -> TestTree
empty mtestDir = askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "Empties the trash" "empty" $ do
    withTests limit $
      property $ do
        testDir <- (</> "e1") <$> liftIO mtestDir
        fileNames <- forAll genFileNameSet
        let pathSet = Set.map (Paths.liftPathI (testDir </>)) fileNames
            fileNamesList = view _MkPathI <$> Set.toList fileNames
            pathsList = (testDir </>) <$> fileNamesList
            trashDir = testDir </> ".trash"
            trashList = (trashDir </>) <$> fileNamesList
            mtrashHome = Just $ MkPathI trashDir

        annotateShow pathsList
        annotateShow trashList

        -- create files and assert existence
        liftIO $ do
          clearDirectory testDir
          createFiles pathsList
        assertFilesExist (view _MkPathI <$> Set.toList pathSet)

        -- delete files
        liftIO $ SafeRm.delete False mtrashHome pathSet

        -- assert original files moved to trash
        assertFilesExist trashList
        assertFilesDoNotExist pathsList

        -- empty trash
        liftIO $ SafeRm.empty mtrashHome

        -- get index
        index <- view #unIndex <$> SafeRm.getIndex mtrashHome
        annotateShow index

        assert $ Map.null index
        assertFilesDoNotExist pathsList
        assertFilesDoNotExist trashList

genFileNameSet :: Gen (HashSet (PathI OriginalPath))
genFileNameSet = Set.fromList <$> Gen.list range genPaths
  where
    range = Range.linear 0 100

gen2FileNameSets :: Gen (HashSet FilePath, HashSet FilePath)
gen2FileNameSets = do
  l <- Set.fromList <$> Gen.list range genRawPaths
  r <- Set.fromList <$> Gen.list range (genRawPathsNoDupes l)
  pure (l, r)
  where
    range = Range.linear 1 100

genPaths :: Gen (PathI OriginalPath)
genPaths = MkPathI <$> genRawPaths

genRawPaths :: Gen FilePath
genRawPaths = Gen.string range genChars
  where
    range = Range.linear 1 20
    genChars = Gen.filterT (not . badChars) Gen.unicode
    badChars c = Ch.isControl c || c == '/'

genRawPathsNoDupes :: HashSet FilePath -> Gen FilePath
genRawPathsNoDupes paths =
  Gen.filter
    (not . flip Set.member paths)
    (Gen.string range genChars)
  where
    range = Range.linear 1 20
    genChars = Gen.filterT (not . badChars) Gen.unicode
    badChars c = Ch.isControl c || c == '/'

toOrigPath :: HashSet FilePath -> PathData -> HashSet FilePath
toOrigPath acc pd = Set.insert (pd ^. #originalPath % _MkPathI) acc
