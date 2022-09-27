-- | Tests for x command.
--
-- @since 0.1
module Functional.Commands.X
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

  -- delete to trash first
  runDel delArgList

  -- list output assertions
  delResult <- captureDel ["l", "-t", trashDir]
  assertMatches expectedDel delResult

  -- file assertions
  assertFilesExist [trashDir </> "f1", trashDir </> ".index.csv"]
  assertDirectoriesExist [trashDir]

  -- PERMANENT DELETE

  let permDelArgList = ["x", "f1", "-f", "-t", trashDir]
  runDel permDelArgList

  -- list output assertions
  permDelResult <- captureDel ["l", "-t", trashDir]
  assertMatches expectedPermDel permDelResult

  -- file assertions
  assertFilesExist [trashDir </> ".index.csv"]
  assertFilesDoNotExist [trashDir </> "f1"]
  assertDirectoriesExist [trashDir]

  where
    expectedDel =
      [ Exact "type:      File",
        Exact "name:      f1",
        Outfix "original:" "/del/x1/f1",
        Prefix "created:",
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

  runDel delArgList

  -- list output assertions
  resultDel <- captureDel ["l", "-t", trashDir]
  assertMatches expectedDel resultDel

  -- file assertions
  assertFilesExist
    ( (trashDir </>)
        <$> [".index.csv", "f1", "f2", "f3", "dir2/dir3/foo"]
    )
  assertDirectoriesExist ((trashDir </>) <$> ["", "dir1", "dir2", "dir2/dir3"])

  -- PERMANENT DELETE

  let permDelArgList =
        ["x", "f1", "f2", "f3", "dir1", "dir2", "-f", "-t", trashDir]
  runDel permDelArgList

  -- list output assertions
  permDelResult <- captureDel ["l", "-t", trashDir]
  assertMatches expectedPermDel permDelResult

  -- file assertions
  assertFilesExist [trashDir </> ".index.csv"]
  assertFilesDoNotExist ((trashDir </>) <$> (filesToDelete <> dirsToDelete))
  assertDirectoriesExist [trashDir]
  where
    expectedDel =
      [ Exact "type:      Directory",
        Exact "name:      dir1",
        Outfix "original:" "/del/x2/dir1",
        Prefix "created:",
        Exact "",
        Exact "type:      Directory",
        Exact "name:      dir2",
        Outfix "original:" "/del/x2/dir2",
        Prefix "created:",
        Exact "",
        Exact "type:      File",
        Exact "name:      f1",
        Outfix "original:" "/del/x2/f1",
        Prefix "created:",
        Exact "",
        Exact "type:      File",
        Exact "name:      f2",
        Outfix "original:" "/del/x2/f2",
        Prefix "created:",
        Exact "",
        Exact "type:      File",
        Exact "name:      f3",
        Outfix "original:" "/del/x2/f3",
        Prefix "created:",
        Exact "Entries:      5",
        Exact "Total Files:  4",
        Prefix "Size:"
      ]
    expectedPermDel =
      [ Exact "Entries:      0",
        Exact "Total Files:  0",
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
  -- up (i.e. dir exists w/ index), so that we can test the perm del
  -- failure only.
  clearDirectory testDir
  createFiles [f1]

  -- delete to trash first
  runDel delArgList

  -- list output assertions
  delResult <- captureDel ["l", "-t", trashDir]
  assertMatches expectedDel delResult

  -- file assertions
  assertFilesExist [trashDir </> "f1", trashDir </> ".index.csv"]
  assertDirectoriesExist [trashDir]

  -- PERMANENT DELETE
  let permDelArgList =
        ["x", "bad file", "-f", "-t", trashDir]

  -- assert exception
  result <-
    (runDel permDelArgList $> Nothing)
      `catch` \(e :: PathNotFoundError) -> pure (Just e)
  case result of
    Nothing -> assertFailure "Expected exception"
    Just ex -> assertMatches expectedPermDel [T.pack $ displayException ex]
  where
    expectedDel =
      [ Exact "type:      File",
        Exact "name:      f1",
        Outfix "original:" "/del/x3/f1",
        Prefix "created:",
        Exact "Entries:      1",
        Exact "Total Files:  1",
        Prefix "Size:"
      ]
    expectedPermDel = [Outfix "Path not found:" "bad file"]