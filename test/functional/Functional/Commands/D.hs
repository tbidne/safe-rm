-- | Tests for d command.
--
-- @since 0.1
module Functional.Commands.D
  ( tests,
  )
where

import Data.Text qualified as T
import Del.Exceptions (PathNotFoundError)
import Functional.Prelude
import Functional.TestArgs (TestArgs (..))

-- | @since 0.1
tests :: IO TestArgs -> TestTree
tests args =
  testGroup
    "Delete (d)"
    [ deletesOne args,
      deletesMany args,
      deleteUnknownError args,
      deleteDuplicateFile args
    ]

deletesOne :: IO TestArgs -> TestTree
deletesOne args = testCase "Deletes a single file" $ do
  tmpDir <- view #tmpDir <$> args
  let testDir = tmpDir </> "d1"
      trashDir = testDir </> ".trash"
      f1 = testDir </> "f1"
      argList = ["d", f1, "-t", trashDir]

  -- setup
  clearDirectory testDir
  createFiles [f1]

  runDel argList

  -- list output assertions
  result <- captureDel ["l", "-t", trashDir]
  assertMatches expected result

  -- file assertions
  assertFilesExist [trashDir </> "f1", trashDir </> ".index.csv"]
  assertDirectoriesExist [trashDir]
  where
    expected =
      [ Exact "type:     File",
        Outfix "trash:" "/del/d1/.trash/f1",
        Outfix "original:" "/del/d1/f1",
        Exact "Entries:      1",
        Exact "Total Files:  1",
        Prefix "Size:"
      ]

deletesMany :: IO TestArgs -> TestTree
deletesMany args = testCase "Deletes several paths" $ do
  tmpDir <- view #tmpDir <$> args
  let testDir = tmpDir </> "d2"
      trashDir = testDir </> ".trash"
      filesToDelete = (testDir </>) <$> ["f1", "f2", "f3"]
      dirsToDelete = (testDir </>) <$> ["dir1", "dir2"]
      argList = ("d" : filesToDelete <> dirsToDelete) <> ["-t", trashDir]

  -- setup
  clearDirectory testDir
  -- test w/ a nested dir
  createDirectories ((testDir </>) <$> ["dir1", "dir2/dir3"])
  -- test w/ a file in dir
  createFiles ((testDir </> "dir2/dir3/foo") : filesToDelete)

  runDel argList

  -- list output assertions
  result <- captureDel ["l", "-t", trashDir]
  assertMatches expected result

  -- file assertions
  assertFilesExist
    ( (trashDir </>)
        <$> [".index.csv", "f1", "f2", "f3", "dir2/dir3/foo"]
    )
  assertDirectoriesExist ((trashDir </>) <$> ["", "dir1", "dir2", "dir2/dir3"])
  where
    expected =
      [ Exact "type:     Directory",
        Outfix "trash:" "/del/d2/.trash/dir1",
        Outfix "original:" "/del/d2/dir1",
        Exact "",
        Exact "type:     File",
        Outfix "trash:" "/del/d2/.trash/f3",
        Outfix "original:" "/del/d2/f3",
        Exact "",
        Exact "type:     File",
        Outfix "trash:" "/del/d2/.trash/f1",
        Outfix "original:" "/del/d2/f1",
        Exact "",
        Exact "type:     File",
        Outfix "trash:" "/del/d2/.trash/f2",
        Outfix "original:" "/del/d2/f2",
        Exact "",
        Exact "type:     Directory",
        Outfix "trash:" "/del/d2/.trash/dir2",
        Outfix "original:" "/del/d2/dir2",
        Exact "Entries:      5",
        Exact "Total Files:  4",
        Prefix "Size:"
      ]

deleteUnknownError :: IO TestArgs -> TestTree
deleteUnknownError args = testCase "Delete unknown prints error" $ do
  tmpDir <- view #tmpDir <$> args
  let testDir = tmpDir </> "d3"
      trashDir = testDir </> ".trash"
      file = testDir </> "bad file"
      argList = ["d", file, "-t", trashDir]

  -- setup
  clearDirectory testDir

  -- assert exception
  result <-
    (runDel argList $> Nothing)
      `catch` \(e :: PathNotFoundError) -> pure (Just e)
  case result of
    Nothing -> assertFailure "Expected exception"
    Just ex -> assertMatches expected [T.pack $ displayException ex]
  where
    expected = [Outfix "Path not found:" "/del/d3/bad file"]

deleteDuplicateFile :: IO TestArgs -> TestTree
deleteDuplicateFile args = testCase "Deletes duplicate file" $ do
  tmpDir <- view #tmpDir <$> args
  let testDir = tmpDir </> "d4"
      trashDir = testDir </> ".trash"
      file = testDir </> "f1"
      argList = ["d", file, "-t", trashDir]

  -- setup
  clearDirectory testDir

  -- create and delete twice
  createFiles [file]
  runDel argList
  createFiles [file]
  runDel argList

  result <- captureDel ["l", "-t", trashDir]
  assertMatches expected result

  -- file assertions
  assertFilesExist
    [trashDir </> "f11", trashDir </> "f1", trashDir </> ".index.csv"]
  assertDirectoriesExist [trashDir]
  where
    expected =
      [ Exact "type:     File",
        Outfix "trash:" "/del/d4/.trash/f11",
        Outfix "original:" "/del/d4/f1",
        Exact "",
        Exact "type:     File",
        Outfix "trash:" "/del/d4/.trash/f1",
        Outfix "original:" "/del/d4/f1",
        Exact "Entries:      2",
        Exact "Total Files:  2",
        Prefix "Size:"
      ]