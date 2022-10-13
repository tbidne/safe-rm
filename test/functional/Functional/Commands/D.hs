-- | Tests for d command.
--
-- @since 0.1
module Functional.Commands.D
  ( tests,
  )
where

import Data.Text qualified as T
import Functional.Prelude

-- | @since 0.1
tests :: TestTree
tests =
  testGroup
    "Delete (d)"
    [ deletesOne,
      deletesMany,
      deleteUnknownError,
      deleteDuplicateFile,
      deletesSome
    ]

deletesOne :: TestTree
deletesOne = testCase "Deletes a single file" $ do
  tmpDir <- getTestDir
  let testDir = tmpDir </> "d1"
      trashDir = testDir </> ".trash"
      f1 = testDir </> "f1"
      argList = ["d", f1, "-t", trashDir]

  -- setup
  clearDirectory testDir
  createFiles [f1]
  assertFilesExist [f1]

  (_, logs) <- captureSafeRmLogs argList
  assertMatches expectedLogs logs

  -- list output assertions
  result <- captureSafeRm ["l", "-t", trashDir]
  assertMatches expected result

  -- file assertions
  assertFilesExist [trashDir </> "f1", trashDir </> ".index.csv"]
  assertFilesDoNotExist [f1]
  assertDirectoriesExist [trashDir]
  where
    expected =
      [ Exact "Type:      File",
        Exact "Name:      f1",
        Outfix "Original:" "/safe-rm/functional/d1/f1",
        Prefix "Created:",
        Exact "Entries:      1",
        Exact "Total Files:  1",
        Prefix "Size:"
      ]
    expectedLogs =
      [ Exact "[2020-05-31 12:00:00][functional.delete][Debug][src/SafeRm.hs:70:4] Trash home: <dir>/d1/.trash",
        Exact "[2020-05-31 12:00:00][functional.delete.deleting][Debug][src/SafeRm.hs:82:10] MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = \"<dir>/d1/f1\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.delete][Info][src/SafeRm.hs:101:4] Deleted: <dir>/d1/f1"
      ]

deletesMany :: TestTree
deletesMany = testCase "Deletes several paths" $ do
  tmpDir <- getTestDir
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
  assertFilesExist filesToDelete
  assertDirectoriesExist dirsToDelete

  (_, logs) <- captureSafeRmLogs argList
  assertMatches expectedLogs logs

  -- list output assertions
  result <- captureSafeRm ["l", "-t", trashDir]
  assertMatches expected result

  -- file assertions
  assertFilesExist
    ( (trashDir </>)
        <$> [".index.csv", "f1", "f2", "f3", "dir2/dir3/foo"]
    )
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesExist ((trashDir </>) <$> ["", "dir1", "dir2", "dir2/dir3"])
  where
    expected =
      [ Exact "Type:      Directory",
        Exact "Name:      dir1",
        Outfix "Original:" "/safe-rm/functional/d2/dir1",
        Prefix "Created:",
        Exact "",
        Exact "Type:      Directory",
        Exact "Name:      dir2",
        Outfix "Original:" "/safe-rm/functional/d2/dir2",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f1",
        Outfix "Original:" "/safe-rm/functional/d2/f1",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f2",
        Outfix "Original:" "/safe-rm/functional/d2/f2",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f3",
        Outfix "Original:" "/safe-rm/functional/d2/f3",
        Prefix "Created:",
        Exact "Entries:      5",
        Exact "Total Files:  4",
        Prefix "Size:"
      ]
    expectedLogs =
      [ Exact "[2020-05-31 12:00:00][functional.delete][Debug][src/SafeRm.hs:70:4] Trash home: <dir>/d2/.trash",
        Exact "[2020-05-31 12:00:00][functional.delete.deleting][Debug][src/SafeRm.hs:82:10] MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f3\"}, originalPath = MkPathI {unPathI = \"<dir>/d2/f3\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.delete.deleting][Debug][src/SafeRm.hs:82:10] MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = \"<dir>/d2/f1\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.delete.deleting][Debug][src/SafeRm.hs:82:10] MkPathData {pathType = PathTypeDirectory, fileName = MkPathI {unPathI = \"dir2\"}, originalPath = MkPathI {unPathI = \"<dir>/d2/dir2\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.delete.deleting][Debug][src/SafeRm.hs:82:10] MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f2\"}, originalPath = MkPathI {unPathI = \"<dir>/d2/f2\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.delete.deleting][Debug][src/SafeRm.hs:82:10] MkPathData {pathType = PathTypeDirectory, fileName = MkPathI {unPathI = \"dir1\"}, originalPath = MkPathI {unPathI = \"<dir>/d2/dir1\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.delete][Info][src/SafeRm.hs:101:4] Deleted: <dir>/d2/dir2, <dir>/d2/f2, <dir>/d2/dir1, <dir>/d2/f3, <dir>/d2/f1"
      ]

deleteUnknownError :: TestTree
deleteUnknownError = testCase "Delete unknown prints error" $ do
  tmpDir <- getTestDir
  let testDir = tmpDir </> "d3"
      trashDir = testDir </> ".trash"
      file = testDir </> "bad file"
      argList = ["d", file, "-t", trashDir]

  -- setup
  clearDirectory testDir

  (ex, logs) <- captureSafeRmExceptionLogs @SomeException argList

  -- assert exception
  assertMatches expected (T.lines . T.pack $ displayException ex)
  assertMatches expectedLogs logs
  where
    expected =
      [ Exact "Encountered exception(s)",
        Outfix "- Path not found:" "/safe-rm/functional/d3/bad file"
      ]
    expectedLogs =
      [ Exact "[2020-05-31 12:00:00][functional.delete][Debug][src/SafeRm.hs:70:4] Trash home: <dir>/d3/.trash",
        Exact "[2020-05-31 12:00:00][functional.delete.deleting][Warn][src/SafeRm.hs:87:10] Path not found: <dir>/d3/bad file",
        Exact "[2020-05-31 12:00:00][functional.delete][Info][src/SafeRm.hs:101:4] Deleted:",
        Exact "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs:126:8] MkExceptionI (Proxy ExceptionIndex 'SomeExceptions) (MkExceptionI (Proxy ExceptionIndex 'PathNotFound) \"<dir>/d3/bad file\" :| [])"
      ]

deleteDuplicateFile :: TestTree
deleteDuplicateFile = testCase "Deletes duplicate file" $ do
  tmpDir <- getTestDir
  let testDir = tmpDir </> "d4"
      trashDir = testDir </> ".trash"
      file = testDir </> "f1"
      argList = ["d", file, "-t", trashDir]

  -- setup
  clearDirectory testDir

  -- create and delete twice
  createFiles [file]
  assertFilesExist [file]
  (_, logs1) <- captureSafeRmLogs argList
  assertMatches expectedLogs1 logs1

  createFiles [file]
  assertFilesExist [file]
  (_, logs2) <- captureSafeRmLogs argList
  assertMatches expectedLogs2 logs2

  result <- captureSafeRm ["l", "-t", trashDir]
  assertMatches expected result

  -- file assertions
  assertFilesExist
    [trashDir </> "f1 (1)", trashDir </> "f1", trashDir </> ".index.csv"]
  assertFilesDoNotExist [file]
  assertDirectoriesExist [trashDir]
  where
    expected =
      [ Exact "Type:      File",
        Exact "Name:      f1",
        Outfix "Original:" "/safe-rm/functional/d4/f1",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f1 (1)",
        Outfix "Original:" "/safe-rm/functional/d4/f1",
        Prefix "Created:",
        Exact "Entries:      2",
        Exact "Total Files:  2",
        Prefix "Size:"
      ]
    expectedLogs1 =
      [ Exact "[2020-05-31 12:00:00][functional.delete][Debug][src/SafeRm.hs:70:4] Trash home: <dir>/d4/.trash",
        Exact "[2020-05-31 12:00:00][functional.delete.deleting][Debug][src/SafeRm.hs:82:10] MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = \"<dir>/d4/f1\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.delete][Info][src/SafeRm.hs:101:4] Deleted: <dir>/d4/f1"
      ]
    expectedLogs2 =
      [ Exact "[2020-05-31 12:00:00][functional.delete][Debug][src/SafeRm.hs:70:4] Trash home: <dir>/d4/.trash",
        Exact "[2020-05-31 12:00:00][functional.delete.deleting][Debug][src/SafeRm.hs:82:10] MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1 (1)\"}, originalPath = MkPathI {unPathI = \"<dir>/d4/f1\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.delete][Info][src/SafeRm.hs:101:4] Deleted: <dir>/d4/f1"
      ]

deletesSome :: TestTree
deletesSome = testCase "Deletes some, errors on others" $ do
  tmpDir <- getTestDir
  let testDir = tmpDir </> "d5"
      trashDir = testDir </> ".trash"
      realFiles = (testDir </>) <$> ["f1", "f2", "f5"]
      filesTryDelete = (testDir </>) <$> ["f1", "f2", "f3", "f4", "f5"]
      argList = ("d" : filesTryDelete) <> ["-t", trashDir]

  -- setup
  clearDirectory testDir
  createFiles realFiles
  assertFilesExist realFiles

  (ex, logs) <- captureSafeRmExceptionLogs argList

  assertExceptionMatches expectedExceptions ex
  assertMatches expectedLogs logs

  -- list output assertions
  resultList <- captureSafeRm ["l", "-t", trashDir]
  assertMatches expected resultList

  -- file assertions
  assertFilesExist
    ( (trashDir </>)
        <$> [".index.csv", "f1", "f2", "f5"]
    )
  assertFilesDoNotExist ((trashDir </>) <$> ["f3", "f4"])
  where
    expectedExceptions =
      [ Outfix "- Path not found:" "/safe-rm/functional/d5/f3",
        Outfix "- Path not found:" "/safe-rm/functional/d5/f4"
      ]
    expected =
      [ Exact "Type:      File",
        Exact "Name:      f1",
        Outfix "Original:" "/safe-rm/functional/d5/f1",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f2",
        Outfix "Original:" "/safe-rm/functional/d5/f2",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f5",
        Outfix "Original:" "/safe-rm/functional/d5/f5",
        Prefix "Created:",
        Exact "Entries:      3",
        Exact "Total Files:  3",
        Prefix "Size:"
      ]
    expectedLogs =
      [ Exact "[2020-05-31 12:00:00][functional.delete][Debug][src/SafeRm.hs:70:4] Trash home: <dir>/d5/.trash",
        Exact "[2020-05-31 12:00:00][functional.delete.deleting][Debug][src/SafeRm.hs:82:10] MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f2\"}, originalPath = MkPathI {unPathI = \"<dir>/d5/f2\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.delete.deleting][Debug][src/SafeRm.hs:82:10] MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f5\"}, originalPath = MkPathI {unPathI = \"<dir>/d5/f5\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.delete.deleting][Warn][src/SafeRm.hs:87:10] Path not found: <dir>/d5/f3",
        Exact "[2020-05-31 12:00:00][functional.delete.deleting][Warn][src/SafeRm.hs:87:10] Path not found: <dir>/d5/f4",
        Exact "[2020-05-31 12:00:00][functional.delete.deleting][Debug][src/SafeRm.hs:82:10] MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = \"<dir>/d5/f1\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.delete][Info][src/SafeRm.hs:101:4] Deleted: <dir>/d5/f5, <dir>/d5/f2, <dir>/d5/f1",
        Exact "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs:126:8] MkExceptionI (Proxy ExceptionIndex 'SomeExceptions) (MkExceptionI (Proxy ExceptionIndex 'PathNotFound) \"<dir>/d5/f4\" :| [MkExceptionI (Proxy ExceptionIndex 'PathNotFound) \"<dir>/d5/f3\"])"
      ]
