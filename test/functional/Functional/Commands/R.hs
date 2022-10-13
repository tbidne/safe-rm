-- | Tests for r command.
--
-- @since 0.1
module Functional.Commands.R
  ( tests,
  )
where

import Functional.Prelude
import SafeRm.Exceptions
  ( ExceptionI,
    ExceptionIndex (SomeExceptions),
  )

-- | @since 0.1
tests :: TestTree
tests =
  testGroup
    "Restore (r)"
    [ restoreOne,
      restoreMany,
      restoreUnknownError,
      restoreCollisionError,
      restoresSome
    ]

restoreOne :: TestTree
restoreOne = testCase "Restores a single file" $ do
  tmpDir <- getTestDir
  let testDir = tmpDir </> "r1"
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

  -- RESTORE

  let restoreArgList = ["r", "f1", "-t", trashDir]
  (_, logs) <- captureSafeRmLogs restoreArgList
  assertMatches expectedLogs logs

  -- list output assertions
  restoreResult <- captureSafeRm ["l", "-t", trashDir]
  assertMatches expectedRestore restoreResult

  -- file assertions
  assertFilesExist [f1, trashDir </> ".index.csv"]
  assertFilesDoNotExist [trashDir </> "f1"]
  assertDirectoriesExist [trashDir]
  where
    expectedDel =
      [ Exact "Type:      File",
        Exact "Name:      f1",
        Outfix "Original:" "/safe-rm/functional/r1/f1",
        Prefix "Created:",
        Exact "Entries:      1",
        Exact "Total Files:  1",
        Prefix "Size:"
      ]
    expectedRestore =
      [ Exact "Entries:      0",
        Exact "Total Files:  0",
        Prefix "Size:"
      ]
    expectedLogs =
      [ Exact "[2020-05-31 12:00:00][functional.restore][Debug][src/SafeRm.hs:236:4] Trash home: <dir>/r1/.trash",
        Exact "[2020-05-31 12:00:00][functional.restore.readIndex][Debug][src/SafeRm/Data/Index.hs:103:4] Index path: <dir>/r1/.trash/.index.csv",
        Exact "[2020-05-31 12:00:00][functional.restore.readIndex.readIndexWithFold][Debug][src/SafeRm/Data/Index.hs:108:8] Found: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = \"<dir>/r1/f1\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.restore.restoring][Debug][src/SafeRm.hs:247:10] MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = \"<dir>/r1/f1\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.restore][Info][src/SafeRm.hs:259:4] Restored: <dir>/r1/f1"
      ]

restoreMany :: TestTree
restoreMany = testCase "Restores several paths" $ do
  tmpDir <- getTestDir
  let testDir = tmpDir </> "r2"
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

  assertDirectoriesExist ((testDir </>) <$> ["dir1", "dir2/dir3"])
  assertFilesExist ((testDir </> "dir2/dir3/foo") : filesToDelete)

  runSafeRm delArgList

  -- list output assertions
  resultDel <- captureSafeRm ["l", "-t", trashDir]
  assertMatches expectedDel resultDel

  -- file assertions
  assertFilesExist
    ( (trashDir </>)
        <$> [".index.csv", "f1", "f2", "f3", "dir2/dir3/foo"]
    )
  assertDirectoriesExist ((trashDir </>) <$> ["", "dir1", "dir2", "dir2/dir3"])
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete

  -- RESTORE

  let restoreArgList =
        -- do not restore f2
        ["r", "f1", "f3", "dir1", "dir2", "-t", trashDir]
  (_, logs) <- captureSafeRmLogs restoreArgList
  assertMatches expectedLogs logs

  -- list output assertions
  restoreResult <- captureSafeRm ["l", "-t", trashDir]
  assertMatches expectedRestore restoreResult

  -- file assertions
  assertFilesExist [trashDir </> "f2", trashDir </> ".index.csv"]
  assertDirectoriesExist [trashDir]
  assertFilesDoNotExist ((trashDir </>) <$> ["f1", "f3"])
  assertDirectoriesDoNotExist
    ((trashDir </>) <$> ["dir1", "dir2", "dir2/dir3"])
  where
    expectedDel =
      [ Exact "Type:      Directory",
        Exact "Name:      dir1",
        Outfix "Original:" "/safe-rm/functional/r2/dir1",
        Prefix "Created:",
        Exact "",
        Exact "Type:      Directory",
        Exact "Name:      dir2",
        Outfix "Original:" "/safe-rm/functional/r2/dir2",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f1",
        Outfix "Original:" "/safe-rm/functional/r2/f1",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f2",
        Outfix "Original:" "/safe-rm/functional/r2/f2",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f3",
        Outfix "Original:" "/safe-rm/functional/r2/f3",
        Prefix "Created:",
        Exact "Entries:      5",
        Exact "Total Files:  4",
        Prefix "Size:"
      ]
    expectedRestore =
      [ Exact "Type:      File",
        Exact "Name:      f2",
        Outfix "Original:" "/safe-rm/functional/r2/f2",
        Prefix "Created:",
        Exact "Entries:      1",
        Exact "Total Files:  1",
        Prefix "Size:"
      ]
    expectedLogs =
      [ Exact "[2020-05-31 12:00:00][functional.restore][Debug][src/SafeRm.hs:236:4] Trash home: <dir>/r2/.trash",
        Exact "[2020-05-31 12:00:00][functional.restore.readIndex][Debug][src/SafeRm/Data/Index.hs:103:4] Index path: <dir>/r2/.trash/.index.csv",
        Exact "[2020-05-31 12:00:00][functional.restore.readIndex.readIndexWithFold][Debug][src/SafeRm/Data/Index.hs:108:8] Found: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = \"<dir>/r2/f1\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.restore.readIndex.readIndexWithFold][Debug][src/SafeRm/Data/Index.hs:108:8] Found: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f3\"}, originalPath = MkPathI {unPathI = \"<dir>/r2/f3\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.restore.readIndex.readIndexWithFold][Debug][src/SafeRm/Data/Index.hs:108:8] Found: MkPathData {pathType = PathTypeDirectory, fileName = MkPathI {unPathI = \"dir1\"}, originalPath = MkPathI {unPathI = \"<dir>/r2/dir1\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.restore.readIndex.readIndexWithFold][Debug][src/SafeRm/Data/Index.hs:108:8] Found: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f2\"}, originalPath = MkPathI {unPathI = \"<dir>/r2/f2\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.restore.readIndex.readIndexWithFold][Debug][src/SafeRm/Data/Index.hs:108:8] Found: MkPathData {pathType = PathTypeDirectory, fileName = MkPathI {unPathI = \"dir2\"}, originalPath = MkPathI {unPathI = \"<dir>/r2/dir2\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.restore.restoring][Debug][src/SafeRm.hs:247:10] MkPathData {pathType = PathTypeDirectory, fileName = MkPathI {unPathI = \"dir2\"}, originalPath = MkPathI {unPathI = \"<dir>/r2/dir2\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.restore.restoring][Debug][src/SafeRm.hs:247:10] MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = \"<dir>/r2/f1\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.restore.restoring][Debug][src/SafeRm.hs:247:10] MkPathData {pathType = PathTypeDirectory, fileName = MkPathI {unPathI = \"dir1\"}, originalPath = MkPathI {unPathI = \"<dir>/r2/dir1\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.restore.restoring][Debug][src/SafeRm.hs:247:10] MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f3\"}, originalPath = MkPathI {unPathI = \"<dir>/r2/f3\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.restore][Info][src/SafeRm.hs:259:4] Restored: <dir>/r2/dir2, <dir>/r2/dir1, <dir>/r2/f3, <dir>/r2/f1"
      ]

restoreUnknownError :: TestTree
restoreUnknownError = testCase "Restore unknown prints error" $ do
  tmpDir <- getTestDir
  let testDir = tmpDir </> "r3"
      trashDir = testDir </> ".trash"
      f1 = testDir </> "f1"
      delArgList = ["d", f1, "-t", trashDir]

  -- SETUP

  -- technically we do not need to have anything in the trash to attempt
  -- a restore, but this way we can ensure the trash itself is set
  -- up (i.e. dir exists w/ index), so that we can test the restore
  -- failure only.
  clearDirectory testDir
  createFiles [f1]

  -- delete to trash first
  runSafeRm delArgList

  -- list output assertions
  delResult <- captureSafeRm ["l", "-t", trashDir]
  assertMatches expectedDel delResult

  -- file assertions
  assertFilesExist [trashDir </> "f1", trashDir </> ".index.csv"]
  assertFilesDoNotExist [f1]
  assertDirectoriesExist [trashDir]

  -- RESTORE
  let restoreArgList = ["r", "bad file", "-t", trashDir]
  (ex, logs) <- captureSafeRmExceptionLogs @(ExceptionI SomeExceptions) restoreArgList

  -- assert exception
  assertExceptionMatches expectedRestore ex
  assertMatches expectedLogs logs

  assertFilesExist [trashDir </> "f1", trashDir </> ".index.csv"]
  where
    expectedDel =
      [ Exact "Type:      File",
        Exact "Name:      f1",
        Outfix "Original:" "/safe-rm/functional/r3/f1",
        Prefix "Created:",
        Exact "Entries:      1",
        Exact "Total Files:  1",
        Prefix "Size:"
      ]
    expectedRestore =
      [ Exact "- Path not found: bad file"
      ]
    expectedLogs =
      [ Exact "[2020-05-31 12:00:00][functional.restore][Debug][src/SafeRm.hs:236:4] Trash home: <dir>/r3/.trash",
        Exact "[2020-05-31 12:00:00][functional.restore.readIndex][Debug][src/SafeRm/Data/Index.hs:103:4] Index path: <dir>/r3/.trash/.index.csv",
        Exact "[2020-05-31 12:00:00][functional.restore.readIndex.readIndexWithFold][Debug][src/SafeRm/Data/Index.hs:108:8] Found: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = \"<dir>/r3/f1\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.restore][Info][src/SafeRm.hs:259:4] Restored:",
        Exact "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs:126:8] MkExceptionI (Proxy ExceptionIndex 'SomeExceptions) (MkExceptionI (Proxy ExceptionIndex 'PathNotFound) \"bad file\" :| [])"
      ]

restoreCollisionError :: TestTree
restoreCollisionError = testCase "Restore collision prints error" $ do
  tmpDir <- getTestDir
  let testDir = tmpDir </> "r4"
      trashDir = testDir </> ".trash"
      f1 = testDir </> "f1"
      delArgList = ["d", f1, "-t", trashDir]

  -- SETUP

  clearDirectory testDir
  createFiles [f1]

  -- delete to trash first and recreate
  runSafeRm delArgList
  createFiles [f1]

  -- list output assertions
  delResult <- captureSafeRm ["l", "-t", trashDir]
  assertMatches expectedDel delResult

  -- file assertions
  assertFilesExist [trashDir </> "f1", f1, trashDir </> ".index.csv"]
  assertDirectoriesExist [trashDir]

  -- RESTORE
  let restoreArgList = ["r", "f1", "-t", trashDir]
  (ex, logs) <- captureSafeRmExceptionLogs @(ExceptionI SomeExceptions) restoreArgList

  -- assert exception
  assertExceptionMatches expectedRestore ex
  assertMatches expectedLogs logs

  assertFilesExist [trashDir </> "f1", f1, trashDir </> ".index.csv"]
  where
    expectedDel =
      [ Exact "Type:      File",
        Exact "Name:      f1",
        Outfix "Original:" "/safe-rm/functional/r4/f1",
        Prefix "Created:",
        Exact "Entries:      1",
        Exact "Total Files:  1",
        Prefix "Size:"
      ]
    expectedRestore =
      [ Outfix
          ( "- Cannot restore the trash file 'f1' as one exists at the "
              <> "original location:"
          )
          "/safe-rm/functional/r4/f1"
      ]
    expectedLogs =
      [ Exact "[2020-05-31 12:00:00][functional.restore][Debug][src/SafeRm.hs:236:4] Trash home: <dir>/r4/.trash",
        Exact "[2020-05-31 12:00:00][functional.restore.readIndex][Debug][src/SafeRm/Data/Index.hs:103:4] Index path: <dir>/r4/.trash/.index.csv",
        Exact "[2020-05-31 12:00:00][functional.restore.readIndex.readIndexWithFold][Debug][src/SafeRm/Data/Index.hs:108:8] Found: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = \"<dir>/r4/f1\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.restore.restoring][Debug][src/SafeRm.hs:247:10] MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = \"<dir>/r4/f1\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.restore.restoring][Warn][src/SafeRm.hs:252:10] Cannot restore the trash file 'f1' as one exists at the original location: <dir>/r4/f1",
        Exact "[2020-05-31 12:00:00][functional.restore][Info][src/SafeRm.hs:259:4] Restored:",
        Exact "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs:126:8] MkExceptionI (Proxy ExceptionIndex 'SomeExceptions) (MkExceptionI (Proxy ExceptionIndex 'RestoreCollision) (MkPathI {unPathI = \"f1\"},MkPathI {unPathI = \"<dir>/r4/f1\"}) :| [])"
      ]

restoresSome :: TestTree
restoresSome = testCase "Restores some, errors on others" $ do
  tmpDir <- getTestDir
  let testDir = tmpDir </> "r5"
      trashDir = testDir </> ".trash"
      realFiles = (testDir </>) <$> ["f1", "f2", "f5"]
      filesTryRestore = ["f1", "f2", "f3", "f4", "f5"]
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

  -- RESTORE
  let restoreArgList =
        ("r" : filesTryRestore) <> ["-t", trashDir]
  (ex, logs) <- captureSafeRmExceptionLogs @(ExceptionI SomeExceptions) restoreArgList

  -- assert exception
  assertExceptionMatches expectedExceptions ex
  assertMatches expectedLogs logs

  -- list output assertions
  resultList <- captureSafeRm ["l", "-t", trashDir]
  assertMatches expected resultList

  -- file assertions
  assertFilesDoNotExist ((trashDir </>) <$> ["f1", "f2", "f5"])
  assertFilesDoNotExist ((testDir </>) <$> ["f3", "f4"])
  assertFilesExist ((testDir </>) <$> ["f1", "f2", "f5"])
  where
    expectedDel =
      [ Exact "Type:      File",
        Exact "Name:      f1",
        Outfix "Original:" "/safe-rm/functional/r5/f1",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f2",
        Outfix "Original:" "/safe-rm/functional/r5/f2",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f5",
        Outfix "Original:" "/safe-rm/functional/r5/f5",
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
      [ Exact "[2020-05-31 12:00:00][functional.restore][Debug][src/SafeRm.hs:236:4] Trash home: <dir>/r5/.trash",
        Exact "[2020-05-31 12:00:00][functional.restore.readIndex][Debug][src/SafeRm/Data/Index.hs:103:4] Index path: <dir>/r5/.trash/.index.csv",
        Exact "[2020-05-31 12:00:00][functional.restore.readIndex.readIndexWithFold][Debug][src/SafeRm/Data/Index.hs:108:8] Found: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = \"<dir>/r5/f1\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.restore.readIndex.readIndexWithFold][Debug][src/SafeRm/Data/Index.hs:108:8] Found: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f2\"}, originalPath = MkPathI {unPathI = \"<dir>/r5/f2\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.restore.readIndex.readIndexWithFold][Debug][src/SafeRm/Data/Index.hs:108:8] Found: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f5\"}, originalPath = MkPathI {unPathI = \"<dir>/r5/f5\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.restore.restoring][Debug][src/SafeRm.hs:247:10] MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f2\"}, originalPath = MkPathI {unPathI = \"<dir>/r5/f2\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.restore.restoring][Debug][src/SafeRm.hs:247:10] MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f5\"}, originalPath = MkPathI {unPathI = \"<dir>/r5/f5\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.restore.restoring][Debug][src/SafeRm.hs:247:10] MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = \"<dir>/r5/f1\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.restore][Info][src/SafeRm.hs:259:4] Restored: <dir>/r5/f5, <dir>/r5/f2, <dir>/r5/f1",
        Exact "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs:126:8] MkExceptionI (Proxy ExceptionIndex 'SomeExceptions) (MkExceptionI (Proxy ExceptionIndex 'PathNotFound) \"f4\" :| [MkExceptionI (Proxy ExceptionIndex 'PathNotFound) \"f3\"])"
      ]
