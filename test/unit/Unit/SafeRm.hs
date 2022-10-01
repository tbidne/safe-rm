{-# OPTIONS_GHC -Wwarn #-}

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
    liftPathI,
    _MkPathI,
  )
import Unit.MaxRuns (MaxRuns (MkMaxRuns))
import Unit.Prelude

tests :: IO FilePath -> TestTree
tests testDir =
  testGroup
    "SafeRm"
    [ delete testDir
    ]

delete :: IO FilePath -> TestTree
delete mtestDir = askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "Paths are deleted" "delete" $ do
    withTests limit $
      property $ do
        testDir <- (</> "1") <$> liftIO mtestDir
        fileNames <- forAll genFileNameSet
        let pathSet = Set.map (liftPathI (testDir </>)) fileNames
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
