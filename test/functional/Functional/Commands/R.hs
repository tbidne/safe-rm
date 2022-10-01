-- | Tests for r command.
--
-- @since 0.1
module Functional.Commands.R
  ( tests,
  )
where

import Data.Text qualified as T
import Functional.Prelude
import Functional.TestArgs (TestArgs (tmpDir))
import SafeRm.Exceptions
  ( ExceptionI,
    ExceptionIndex (SomeExceptions),
  )

-- | @since 0.1
tests :: IO TestArgs -> TestTree
tests args =
  testGroup
    "Restore (r)"
    [ restoreOne args,
      restoreMany args,
      restoreUnknownError args,
      restoreCollisionError args,
      restoresSome args
    ]

restoreOne :: IO TestArgs -> TestTree
restoreOne args = testCase "Restores a single file" $ do
  tmpDir <- view #tmpDir <$> args
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
  runSafeRm restoreArgList

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
        Outfix "Original:" "/safe-rm/r1/f1",
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

restoreMany :: IO TestArgs -> TestTree
restoreMany args = testCase "Restores several paths" $ do
  tmpDir <- view #tmpDir <$> args
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
  runSafeRm restoreArgList

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
        Outfix "Original:" "/safe-rm/r2/dir1",
        Prefix "Created:",
        Exact "",
        Exact "Type:      Directory",
        Exact "Name:      dir2",
        Outfix "Original:" "/safe-rm/r2/dir2",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f1",
        Outfix "Original:" "/safe-rm/r2/f1",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f2",
        Outfix "Original:" "/safe-rm/r2/f2",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f3",
        Outfix "Original:" "/safe-rm/r2/f3",
        Prefix "Created:",
        Exact "Entries:      5",
        Exact "Total Files:  4",
        Prefix "Size:"
      ]
    expectedRestore =
      [ Exact "Type:      File",
        Exact "Name:      f2",
        Outfix "Original:" "/safe-rm/r2/f2",
        Prefix "Created:",
        Exact "Entries:      1",
        Exact "Total Files:  1",
        Prefix "Size:"
      ]

restoreUnknownError :: IO TestArgs -> TestTree
restoreUnknownError args = testCase "Restore unknown prints error" $ do
  tmpDir <- view #tmpDir <$> args
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

  -- assert exception
  result <-
    (runSafeRm restoreArgList $> Nothing)
      `catch` \(e :: ExceptionI SomeExceptions) -> pure (Just e)
  case result of
    Nothing -> assertFailure "Expected exception"
    Just ex ->
      assertMatches
        expectedRestore
        (T.lines . T.pack $ displayException ex)
  assertFilesExist [trashDir </> "f1", trashDir </> ".index.csv"]
  where
    expectedDel =
      [ Exact "Type:      File",
        Exact "Name:      f1",
        Outfix "Original:" "/safe-rm/r3/f1",
        Prefix "Created:",
        Exact "Entries:      1",
        Exact "Total Files:  1",
        Prefix "Size:"
      ]
    expectedRestore =
      [ Exact "Encountered exception(s)",
        Exact "- Path not found: bad file"
      ]

restoreCollisionError :: IO TestArgs -> TestTree
restoreCollisionError args = testCase "Restore collision prints error" $ do
  tmpDir <- view #tmpDir <$> args
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

  -- assert exception
  result <-
    (runSafeRm restoreArgList $> Nothing)
      `catch` \(e :: ExceptionI SomeExceptions) -> pure (Just e)
  case result of
    Nothing -> assertFailure "Expected exception"
    Just ex ->
      assertMatches
        expectedRestore
        (T.lines . T.pack $ displayException ex)
  assertFilesExist [trashDir </> "f1", f1, trashDir </> ".index.csv"]
  where
    expectedDel =
      [ Exact "Type:      File",
        Exact "Name:      f1",
        Outfix "Original:" "/safe-rm/r4/f1",
        Prefix "Created:",
        Exact "Entries:      1",
        Exact "Total Files:  1",
        Prefix "Size:"
      ]
    expectedRestore =
      [ Exact "Encountered exception(s)",
        Outfix
          ( "- Cannot restore the trash file 'f1' as one exists at the "
              <> "original location:"
          )
          "/safe-rm/r4/f1"
      ]

restoresSome :: IO TestArgs -> TestTree
restoresSome args = testCase "Restores some, errors on others" $ do
  tmpDir <- view #tmpDir <$> args
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

  -- PERMANENT DELETE
  let permDelArgList =
        ("r" : filesTryRestore) <> ["-t", trashDir]

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
  assertFilesDoNotExist ((trashDir </>) <$> ["f1", "f2", "f5"])
  assertFilesDoNotExist ((testDir </>) <$> ["f3", "f4"])
  assertFilesExist ((testDir </>) <$> ["f1", "f2", "f5"])
  where
    expectedDel =
      [ Exact "Type:      File",
        Exact "Name:      f1",
        Outfix "Original:" "/safe-rm/r5/f1",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f2",
        Outfix "Original:" "/safe-rm/r5/f2",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f5",
        Outfix "Original:" "/safe-rm/r5/f5",
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
