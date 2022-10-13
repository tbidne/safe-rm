-- | Tests for x command.
--
-- @since 0.1
module Functional.Commands.X
  ( tests,
  )
where

import Functional.Prelude
import SafeRm.Exceptions (ExceptionI, ExceptionIndex (SomeExceptions))

-- | @since 0.1
tests :: TestTree
tests =
  testGroup
    "Permanent Delete (x)"
    [ deletesOne,
      deletesMany,
      deleteUnknownError,
      deletesSome
    ]

deletesOne :: TestTree
deletesOne = testCase "Permanently deletes a single file" $ do
  tmpDir <- getTestDir
  let testDir = tmpDir </> "x1"
      trashDir = testDir </> ".trash"
      f1 = testDir </> "f1"
      delArgList = ["d", f1, "-t", trashDir]

  -- SETUP

  clearDirectory testDir
  createFiles [f1]
  assertFilesExist [f1]

  -- delete to trash first
  runSafeRm delArgList

  -- list output assertions
  delResult <- captureSafeRm ["l", "-t", trashDir]
  assertMatches expectedDel delResult

  -- file assertions
  assertFilesExist [trashDir </> "f1", trashDir </> ".index.csv"]
  assertFilesDoNotExist [f1]
  assertDirectoriesExist [trashDir]

  -- PERMANENT DELETE

  let permDelArgList = ["x", "f1", "-f", "-t", trashDir]
  (_, logs) <- captureSafeRmLogs permDelArgList
  assertMatches expectedLogs logs

  -- list output assertions
  permDelResult <- captureSafeRm ["l", "-t", trashDir]
  assertMatches expectedPermDel permDelResult

  -- file assertions
  assertFilesExist [trashDir </> ".index.csv"]
  assertFilesDoNotExist [f1, trashDir </> "f1"]
  assertDirectoriesExist [trashDir]
  where
    expectedDel =
      [ Exact "Type:      File",
        Exact "Name:      f1",
        Outfix "Original:" "/safe-rm/functional/x1/f1",
        Prefix "Created:",
        Exact "Entries:      1",
        Exact "Total Files:  1",
        Prefix "Size:"
      ]
    expectedPermDel =
      [ Exact "Entries:      0",
        Exact "Total Files:  0",
        Prefix "Size:"
      ]
    expectedLogs =
      [ Exact "[2020-05-31 12:00:00][functional.deletePermanently][Debug][src/SafeRm.hs:127:4] Trash home: <dir>/x1/.trash",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.readIndex][Debug][src/SafeRm/Data/Index.hs:103:4] Index path: <dir>/x1/.trash/.index.csv",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.readIndex.readIndexWithFold][Debug][src/SafeRm/Data/Index.hs:108:8] Found: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = \"<dir>/x1/f1\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.deleting][Debug][src/SafeRm.hs:137:14] MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = \"<dir>/x1/f1\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently][Info][src/SafeRm.hs:173:4] Deleted: f1"
      ]

deletesMany :: TestTree
deletesMany = testCase "Permanently deletes several paths" $ do
  tmpDir <- getTestDir
  let testDir = tmpDir </> "x2"
      trashDir = testDir </> ".trash"
      filesToDelete = (testDir </>) <$> ["f1", "f2", "f3"]
      dirsToDelete = (testDir </>) <$> ["dir1", "dir2"]
      delArgList = ("d" : filesToDelete <> dirsToDelete) <> ["-t", trashDir]

  -- SETUP
  clearDirectory testDir
  -- test w/ a nested dir
  createDirectories ((testDir </>) <$> ["dir1", "dir2/dir3"])
  -- test w/ a file in dir
  createFiles ((testDir </> "dir2/dir3/foo") : filesToDelete)
  assertFilesExist filesToDelete
  assertDirectoriesExist dirsToDelete

  runSafeRm delArgList

  -- list output assertions
  resultDel <- captureSafeRm ["l", "-t", trashDir]
  assertMatches expectedDel resultDel

  -- file assertions
  assertFilesExist
    ( (trashDir </>)
        <$> [".index.csv", "f1", "f2", "f3", "dir2/dir3/foo"]
    )
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesExist ((trashDir </>) <$> ["", "dir1", "dir2", "dir2/dir3"])

  -- PERMANENT DELETE

  let permDelArgList =
        -- leave f2 alone
        ["x", "f1", "f3", "dir1", "dir2", "-f", "-t", trashDir]
  (_, logs) <- captureSafeRmLogs permDelArgList
  assertMatches expectedLogs logs

  -- list output assertions
  permDelResult <- captureSafeRm ["l", "-t", trashDir]
  assertMatches expectedPermDel permDelResult

  -- file assertions
  assertFilesDoNotExist
    ( (trashDir </>)
        <$> filesToDelete
          <> filesToDelete
    )
  assertDirectoriesDoNotExist
    ((testDir </>) <$> ["dir1", "dir2/dir3"] <> dirsToDelete)
  assertFilesExist [trashDir </> "f2", trashDir </> ".index.csv"]
  assertDirectoriesExist [trashDir]
  where
    expectedDel =
      [ Exact "Type:      Directory",
        Exact "Name:      dir1",
        Outfix "Original:" "/safe-rm/functional/x2/dir1",
        Prefix "Created:",
        Exact "",
        Exact "Type:      Directory",
        Exact "Name:      dir2",
        Outfix "Original:" "/safe-rm/functional/x2/dir2",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f1",
        Outfix "Original:" "/safe-rm/functional/x2/f1",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f2",
        Outfix "Original:" "/safe-rm/functional/x2/f2",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f3",
        Outfix "Original:" "/safe-rm/functional/x2/f3",
        Prefix "Created:",
        Exact "Entries:      5",
        Exact "Total Files:  4",
        Prefix "Size:"
      ]
    expectedPermDel =
      [ Exact "Type:      File",
        Exact "Name:      f2",
        Outfix "Original:" "/safe-rm/functional/x2/f2",
        Prefix "Created:",
        Exact "Entries:      1",
        Exact "Total Files:  1",
        Prefix "Size:"
      ]
    expectedLogs =
      [ Exact "[2020-05-31 12:00:00][functional.deletePermanently][Debug][src/SafeRm.hs:127:4] Trash home: <dir>/x2/.trash",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.readIndex][Debug][src/SafeRm/Data/Index.hs:103:4] Index path: <dir>/x2/.trash/.index.csv",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.readIndex.readIndexWithFold][Debug][src/SafeRm/Data/Index.hs:108:8] Found: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = \"<dir>/x2/f1\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.readIndex.readIndexWithFold][Debug][src/SafeRm/Data/Index.hs:108:8] Found: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f3\"}, originalPath = MkPathI {unPathI = \"<dir>/x2/f3\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.readIndex.readIndexWithFold][Debug][src/SafeRm/Data/Index.hs:108:8] Found: MkPathData {pathType = PathTypeDirectory, fileName = MkPathI {unPathI = \"dir1\"}, originalPath = MkPathI {unPathI = \"<dir>/x2/dir1\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.readIndex.readIndexWithFold][Debug][src/SafeRm/Data/Index.hs:108:8] Found: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f2\"}, originalPath = MkPathI {unPathI = \"<dir>/x2/f2\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.readIndex.readIndexWithFold][Debug][src/SafeRm/Data/Index.hs:108:8] Found: MkPathData {pathType = PathTypeDirectory, fileName = MkPathI {unPathI = \"dir2\"}, originalPath = MkPathI {unPathI = \"<dir>/x2/dir2\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.deleting][Debug][src/SafeRm.hs:137:14] MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f3\"}, originalPath = MkPathI {unPathI = \"<dir>/x2/f3\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.deleting][Debug][src/SafeRm.hs:137:14] MkPathData {pathType = PathTypeDirectory, fileName = MkPathI {unPathI = \"dir1\"}, originalPath = MkPathI {unPathI = \"<dir>/x2/dir1\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.deleting][Debug][src/SafeRm.hs:137:14] MkPathData {pathType = PathTypeDirectory, fileName = MkPathI {unPathI = \"dir2\"}, originalPath = MkPathI {unPathI = \"<dir>/x2/dir2\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.deleting][Debug][src/SafeRm.hs:137:14] MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = \"<dir>/x2/f1\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently][Info][src/SafeRm.hs:173:4] Deleted: dir2, dir1, f3, f1"
      ]

deleteUnknownError :: TestTree
deleteUnknownError = testCase "Delete unknown prints error" $ do
  tmpDir <- getTestDir
  let testDir = tmpDir </> "x3"
      trashDir = testDir </> ".trash"
      f1 = testDir </> "f1"
      delArgList = ["d", f1, "-t", trashDir]

  -- SETUP

  -- technically we do not need to have anything in the trash to attempt
  -- a permanent delete, but this way we can ensure the trash itself is set
  -- up (i.e. dir exists w/ index), so that we can test the perm safe-rm
  -- failure only.
  clearDirectory testDir
  createFiles [f1]
  assertFilesExist [f1]

  -- delete to trash first
  runSafeRm delArgList

  -- list output assertions
  delResult <- captureSafeRm ["l", "-t", trashDir]
  assertMatches expectedDel delResult

  -- file assertions
  assertFilesExist [trashDir </> "f1", trashDir </> ".index.csv"]
  assertFilesDoNotExist [f1]
  assertDirectoriesExist [trashDir]

  -- PERMANENT DELETE
  let permDelArgList =
        ["x", "bad file", "-f", "-t", trashDir]
  (ex, logs) <- captureSafeRmExceptionLogs @(ExceptionI SomeExceptions) permDelArgList

  -- assert exception
  assertExceptionMatches expectedPermDel ex
  assertMatches expectedLogs logs

  assertFilesExist [trashDir </> "f1", trashDir </> ".index.csv"]
  where
    expectedDel =
      [ Exact "Type:      File",
        Exact "Name:      f1",
        Outfix "Original:" "/safe-rm/functional/x3/f1",
        Prefix "Created:",
        Exact "Entries:      1",
        Exact "Total Files:  1",
        Prefix "Size:"
      ]
    expectedPermDel =
      [ Exact "- Path not found: bad file"
      ]
    expectedLogs =
      [ Exact "[2020-05-31 12:00:00][functional.deletePermanently][Debug][src/SafeRm.hs:127:4] Trash home: <dir>/x3/.trash",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.readIndex][Debug][src/SafeRm/Data/Index.hs:103:4] Index path: <dir>/x3/.trash/.index.csv",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.readIndex.readIndexWithFold][Debug][src/SafeRm/Data/Index.hs:108:8] Found: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = \"<dir>/x3/f1\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently][Info][src/SafeRm.hs:173:4] Deleted:",
        Exact "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs:126:8] MkExceptionI (Proxy ExceptionIndex 'SomeExceptions) (MkExceptionI (Proxy ExceptionIndex 'PathNotFound) \"bad file\" :| [])"
      ]

deletesSome :: TestTree
deletesSome = testCase "Deletes some, errors on others" $ do
  tmpDir <- getTestDir
  let testDir = tmpDir </> "x4"
      trashDir = testDir </> ".trash"
      realFiles = (testDir </>) <$> ["f1", "f2", "f5"]
      filesTryPermDelete = ["f1", "f2", "f3", "f4", "f5"]
      delArgList = ("d" : realFiles) <> ["-t", trashDir]

  -- setup
  clearDirectory testDir
  createFiles realFiles
  assertFilesExist realFiles

  -- delete to trash first
  runSafeRm delArgList

  -- list output assertions
  delResult <- captureSafeRm ["l", "-t", trashDir]
  assertMatches expectedDel delResult

  -- file assertions
  assertFilesExist ((trashDir </>) <$> ["f1", "f2", "f5"])
  assertFilesDoNotExist realFiles
  assertDirectoriesExist [trashDir]

  -- PERMANENT DELETE
  let permDelArgList =
        ("x" : filesTryPermDelete) <> ["-f", "-t", trashDir]
  (ex, logs) <- captureSafeRmExceptionLogs @(ExceptionI SomeExceptions) permDelArgList

  -- assert exception
  assertExceptionMatches expectedExceptions ex
  assertMatches expectedLogs logs

  -- list output assertions
  resultList <- captureSafeRm ["l", "-t", trashDir]
  assertMatches expected resultList

  -- file assertions
  assertFilesDoNotExist ((trashDir </>) <$> filesTryPermDelete)
  where
    expectedDel =
      [ Exact "Type:      File",
        Exact "Name:      f1",
        Outfix "Original:" "/safe-rm/functional/x4/f1",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f2",
        Outfix "Original:" "/safe-rm/functional/x4/f2",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f5",
        Outfix "Original:" "/safe-rm/functional/x4/f5",
        Prefix "Created:",
        Exact "Entries:      3",
        Exact "Total Files:  3",
        Prefix "Size:"
      ]
    expectedExceptions =
      [ Exact "- Path not found: f3",
        Exact "- Path not found: f4"
      ]
    expected =
      [ Exact "Entries:      0",
        Exact "Total Files:  0",
        Prefix "Size:"
      ]
    expectedLogs =
      [ Exact "[2020-05-31 12:00:00][functional.deletePermanently][Debug][src/SafeRm.hs:127:4] Trash home: <dir>/x4/.trash",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.readIndex][Debug][src/SafeRm/Data/Index.hs:103:4] Index path: <dir>/x4/.trash/.index.csv",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.readIndex.readIndexWithFold][Debug][src/SafeRm/Data/Index.hs:108:8] Found: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = \"<dir>/x4/f1\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.readIndex.readIndexWithFold][Debug][src/SafeRm/Data/Index.hs:108:8] Found: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f2\"}, originalPath = MkPathI {unPathI = \"<dir>/x4/f2\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.readIndex.readIndexWithFold][Debug][src/SafeRm/Data/Index.hs:108:8] Found: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f5\"}, originalPath = MkPathI {unPathI = \"<dir>/x4/f5\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.deleting][Debug][src/SafeRm.hs:137:14] MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = \"<dir>/x4/f1\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.deleting][Debug][src/SafeRm.hs:137:14] MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f2\"}, originalPath = MkPathI {unPathI = \"<dir>/x4/f2\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently.deleting][Debug][src/SafeRm.hs:137:14] MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f5\"}, originalPath = MkPathI {unPathI = \"<dir>/x4/f5\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.deletePermanently][Info][src/SafeRm.hs:173:4] Deleted: f5, f2, f1",
        Exact "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs:126:8] MkExceptionI (Proxy ExceptionIndex 'SomeExceptions) (MkExceptionI (Proxy ExceptionIndex 'PathNotFound) \"f4\" :| [MkExceptionI (Proxy ExceptionIndex 'PathNotFound) \"f3\"])"
      ]
