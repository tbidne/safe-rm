-- | Tests for x command.
--
-- @since 0.1
module Functional.Commands.X
  ( tests,
  )
where

import Data.Text qualified as T
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
  runSafeRm permDelArgList

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
  runSafeRm permDelArgList

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

  -- assert exception
  result <-
    (runSafeRm permDelArgList $> Nothing)
      `catch` \(e :: ExceptionI SomeExceptions) -> pure (Just e)
  case result of
    Nothing -> assertFailure "Expected exception"
    Just ex ->
      assertMatches
        expectedPermDel
        (T.lines . T.pack $ displayException ex)
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
      [ Exact "Encountered exception(s)",
        Exact "- Path not found: bad file"
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

  result <-
    (runSafeRm permDelArgList $> Nothing)
      `catch` \(e :: ExceptionI SomeExceptions) -> pure (Just e)
  case result of
    Nothing -> assertFailure "Expected exception"
    Just ex -> assertExceptionMatches expectedExceptions ex

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
