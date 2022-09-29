-- | Tests for x command.
--
-- @since 0.1
module Functional.Commands.X
  ( tests,
  )
where

import Data.Text qualified as T
import Functional.Prelude
import Functional.TestArgs (TestArgs (tmpDir))
import SafeRm.Exceptions (ExceptionI, ExceptionIndex (PathNotFound))

-- | @since 0.1
tests :: IO TestArgs -> TestTree
tests args =
  testGroup
    "Permanent Delete (x)"
    [ deletesOne args,
      deletesMany args,
      deleteUnknownError args
    ]

deletesOne :: IO TestArgs -> TestTree
deletesOne args = testCase "Permanently deletes a single file" $ do
  tmpDir <- view #tmpDir <$> args
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
        Outfix "Original:" "/safe-rm/x1/f1",
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

deletesMany :: IO TestArgs -> TestTree
deletesMany args = testCase "Permanently deletes several paths" $ do
  tmpDir <- view #tmpDir <$> args
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
        Outfix "Original:" "/safe-rm/x2/dir1",
        Prefix "Created:",
        Exact "",
        Exact "Type:      Directory",
        Exact "Name:      dir2",
        Outfix "Original:" "/safe-rm/x2/dir2",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f1",
        Outfix "Original:" "/safe-rm/x2/f1",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f2",
        Outfix "Original:" "/safe-rm/x2/f2",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f3",
        Outfix "Original:" "/safe-rm/x2/f3",
        Prefix "Created:",
        Exact "Entries:      5",
        Exact "Total Files:  4",
        Prefix "Size:"
      ]
    expectedPermDel =
      [ Exact "Type:      File",
        Exact "Name:      f2",
        Outfix "Original:" "/safe-rm/x2/f2",
        Prefix "Created:",
        Exact "Entries:      1",
        Exact "Total Files:  1",
        Prefix "Size:"
      ]

deleteUnknownError :: IO TestArgs -> TestTree
deleteUnknownError args = testCase "Delete unknown prints error" $ do
  tmpDir <- view #tmpDir <$> args
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
      `catch` \(e :: ExceptionI PathNotFound) -> pure (Just e)
  case result of
    Nothing -> assertFailure "Expected exception"
    Just ex -> assertMatches expectedPermDel [T.pack $ displayException ex]
  assertFilesExist [trashDir </> "f1", trashDir </> ".index.csv"]
  where
    expectedDel =
      [ Exact "Type:      File",
        Exact "Name:      f1",
        Outfix "Original:" "/safe-rm/x3/f1",
        Prefix "Created:",
        Exact "Entries:      1",
        Exact "Total Files:  1",
        Prefix "Size:"
      ]
    expectedPermDel = [Outfix "Path not found:" "bad file"]
