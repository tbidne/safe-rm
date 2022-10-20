-- | Tests for x command.
--
-- @since 0.1
module Functional.Commands.X
  ( tests,
  )
where

import Functional.Prelude
import SafeRm.Exception (Exceptions)

-- | @since 0.1
tests :: IO FilePath -> TestTree
tests args =
  testGroup
    "Permanent Delete (x)"
    [ deletesOne args,
      deletesMany args,
      deleteUnknownError args,
      deletesSome args,
      deletesSomeNoTrace args
    ]

deletesOne :: IO FilePath -> TestTree
deletesOne args = goldenVsStringDiff desc diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "x1"
      trashDir = testDir </> ".trash"
      f1 = testDir </> "f1"
      delArgList = ["d", f1, "-t", trashDir]

  -- SETUP

  clearDirectory testDir
  createFiles [f1]
  assertFilesExist [f1]

  -- delete to trash first
  runSafeRm tmpDir delArgList

  -- list output assertions
  delResult <- captureSafeRm tmpDir "LIST 1" ["l", "-t", trashDir]

  -- file assertions
  assertFilesExist [trashDir </> "f1", trashDir </> ".index.csv"]
  assertFilesDoNotExist [f1]
  assertDirectoriesExist [trashDir]

  -- PERMANENT DELETE

  let permDelArgList = ["x", "f1", "-f", "-t", trashDir]
  (_, logs) <- captureSafeRmLogs tmpDir "PERM DELETE" permDelArgList

  -- list output assertions
  permDelResult <- captureSafeRm tmpDir "LIST 2" ["l", "-t", trashDir]

  -- file assertions
  assertFilesExist [trashDir </> ".index.csv"]
  assertFilesDoNotExist [f1, trashDir </> "f1"]
  assertDirectoriesExist [trashDir]
  pure $ capturedToBs [delResult, logs, permDelResult]
  where
    desc = "Permanently deletes a single file"
    gpath = goldenPath </> "single.golden"

deletesMany :: IO FilePath -> TestTree
deletesMany args = goldenVsStringDiff desc diff gpath $ do
  tmpDir <- args
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

  runSafeRm tmpDir delArgList

  -- list output assertions
  delResult <- captureSafeRm tmpDir "LIST 1" ["l", "-t", trashDir]

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
  (_, logs) <- captureSafeRmLogs tmpDir "PERM DELETE" permDelArgList

  -- list output assertions
  permDelResult <- captureSafeRm tmpDir "LIST 2" ["l", "-t", trashDir]

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
  pure $ capturedToBs [delResult, logs, permDelResult]
  where
    desc = "Permanently deletes several paths"
    gpath = goldenPath </> "many.golden"

deleteUnknownError :: IO FilePath -> TestTree
deleteUnknownError args = goldenVsStringDiff desc diff gpath $ do
  tmpDir <- args
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
  runSafeRm tmpDir delArgList

  -- list output assertions
  delResult <- captureSafeRm tmpDir "LIST" ["l", "-t", trashDir]

  -- file assertions
  assertFilesExist [trashDir </> "f1", trashDir </> ".index.csv"]
  assertFilesDoNotExist [f1]
  assertDirectoriesExist [trashDir]

  -- PERMANENT DELETE
  let permDelArgList =
        ["x", "bad file", "-f", "-t", trashDir]
  (ex, logs) <-
    captureSafeRmTraceExceptionLogs
      @Exceptions
      tmpDir
      "PERM DELETE"
      permDelArgList

  -- assert exception
  assertFilesExist [trashDir </> "f1", trashDir </> ".index.csv"]
  pure $ capturedToBs [delResult, ex, logs]
  where
    desc = "Delete unknown prints error"
    gpath = goldenPath </> "unknown.golden"

deletesSome :: IO FilePath -> TestTree
deletesSome args = goldenVsStringDiff desc diff gpath $ do
  tmpDir <- args
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
  runSafeRm tmpDir delArgList

  -- list output assertions
  delResult <- captureSafeRm tmpDir "LIST 1" ["l", "-t", trashDir]

  -- file assertions
  assertFilesExist ((trashDir </>) <$> ["f1", "f2", "f5"])
  assertFilesDoNotExist realFiles
  assertDirectoriesExist [trashDir]

  -- PERMANENT DELETE
  let permDelArgList =
        ("x" : filesTryPermDelete) <> ["-f", "-t", trashDir]
  (ex, logs) <-
    captureSafeRmTraceExceptionLogs
      @Exceptions
      tmpDir
      "PERM DELETE"
      permDelArgList

  -- list output assertions
  resultList <- captureSafeRm tmpDir "LIST 2" ["l", "-t", trashDir]

  -- file assertions
  assertFilesDoNotExist ((trashDir </>) <$> filesTryPermDelete)
  pure $ capturedToBs [delResult, ex, logs, resultList]
  where
    desc = "Deletes some, errors on others"
    gpath = goldenPath </> "some.golden"

deletesSomeNoTrace :: IO FilePath -> TestTree
deletesSomeNoTrace args = goldenVsStringDiff desc diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "x5"
      trashDir = testDir </> ".trash"
      realFiles = (testDir </>) <$> ["f1", "f2", "f5"]
      filesTryPermDelete = ["f1", "f2", "f3", "f4", "f5"]
      delArgList = ("d" : realFiles) <> ["-t", trashDir]

  -- setup
  clearDirectory testDir
  createFiles realFiles
  assertFilesExist realFiles

  -- delete to trash first
  runSafeRm tmpDir delArgList

  -- list output assertions
  delResult <- captureSafeRm tmpDir "LIST 1" ["l", "-t", trashDir]

  -- file assertions
  assertFilesExist ((trashDir </>) <$> ["f1", "f2", "f5"])
  assertFilesDoNotExist realFiles
  assertDirectoriesExist [trashDir]

  -- PERMANENT DELETE
  let permDelArgList =
        ("x" : filesTryPermDelete) <> ["-f", "-t", trashDir]
  (ex, logs) <-
    captureSafeRmExceptionLogs
      @Exceptions
      tmpDir
      "PERM DELETE"
      permDelArgList

  -- list output assertions
  resultList <- captureSafeRm tmpDir "LIST 2" ["l", "-t", trashDir]

  -- file assertions
  assertFilesDoNotExist ((trashDir </>) <$> filesTryPermDelete)
  pure $ capturedToBs [delResult, ex, logs, resultList]
  where
    desc = "Deletes some no trace"
    gpath = goldenPath </> "no-trace.golden"

goldenPath :: FilePath
goldenPath = "test/functional/Functional/Commands/X"
