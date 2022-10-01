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
import Data.Text qualified as T
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
import Unit.MaxRuns (MaxRuns (MkMaxRuns))
import Unit.Prelude

tests :: IO FilePath -> TestTree
tests testDir =
  testGroup
    "SafeRm"
    [ delete testDir,
      deletePermanently testDir,
      restore testDir,
      empty testDir
    ]

delete :: IO FilePath -> TestTree
delete mtestDir = askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "All paths are deleted" "delete" $ do
    withTests limit $
      property $ do
        testDir <- (</> "1") <$> liftIO mtestDir
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
  where
    toOrigPath :: HashSet FilePath -> PathData -> HashSet FilePath
    toOrigPath acc pd = Set.insert (pd ^. #originalPath % _MkPathI) acc

deletePermanently :: IO FilePath -> TestTree
deletePermanently mtestDir = askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "All trash entries are permanently deleted" "deletePermanently" $ do
    withTests limit $
      property $ do
        testDir <- (</> "2") <$> liftIO mtestDir
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
        testDir <- (</> "3") <$> liftIO mtestDir
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
        testDir <- (</> "4") <$> liftIO mtestDir
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

genPaths :: Gen (PathI OriginalPath)
genPaths = do
  MkPathI . T.unpack <$> Gen.text range genChars
  where
    range = Range.linear 1 20
    genChars = Gen.filterT (not . badChars) Gen.unicode
    badChars c = Ch.isControl c || c == '/'
