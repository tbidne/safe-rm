-- | Tests for m command.
--
-- @since 0.1
module Functional.Commands.E
  ( tests,
  )
where

import Functional.Prelude

-- | @since 0.1
tests :: TestTree
tests =
  testGroup
    "Empty (e)"
    [ emptyTrash,
      emptyTrashTwice
    ]

emptyTrash :: TestTree
emptyTrash = testCase "Empties trash" $ do
  tmpDir <- getTestDir
  let testDir = tmpDir </> "e1"
      trashDir = testDir </> ".trash"
      filesToDelete = (testDir </>) <$> ["f1", "f2", "f3"]
      dirsToDelete = (testDir </>) <$> ["dir1", "dir2"]
      delArgList = ("d" : filesToDelete <> dirsToDelete) <> ["-t", trashDir]

  -- setup
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

  -- EMPTY

  let emptyArgList = ["e", "-f", "-t", trashDir]
  (_, logs) <- captureSafeRmLogs emptyArgList
  assertMatches expectedLogs logs

  -- list output assertions
  result <- captureSafeRm ["l", "-t", trashDir]
  assertMatches expectedEmpty result

  -- file assertions
  assertFilesDoNotExist
    ( (trashDir </>)
        <$> [".index.csv", "f1", "f2", "f3", "dir2/dir3/foo"]
    )
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesDoNotExist
    ((trashDir </>) <$> ["", "dir1", "dir2", "dir2/dir3"])
  where
    expectedDel =
      [ Exact "Type:      Directory",
        Exact "Name:      dir1",
        Outfix "Original:" "/safe-rm/functional/e1/dir1",
        Prefix "Created:",
        Exact "",
        Exact "Type:      Directory",
        Exact "Name:      dir2",
        Outfix "Original:" "/safe-rm/functional/e1/dir2",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f1",
        Outfix "Original:" "/safe-rm/functional/e1/f1",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f2",
        Outfix "Original:" "/safe-rm/functional/e1/f2",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f3",
        Outfix "Original:" "/safe-rm/functional/e1/f3",
        Prefix "Created:",
        Exact "Entries:      5",
        Exact "Total Files:  4",
        Prefix "Size:"
      ]
    expectedEmpty =
      [ Exact "Entries:      0",
        Exact "Total Files:  0",
        Prefix "Size:"
      ]
    expectedLogs =
      [ Exact "[2020-05-31 12:00:00][functional.emptyTrash][Debug][src/SafeRm.hs:280:4] Trash home: <dir>/e1/.trash"
      ]

emptyTrashTwice :: TestTree
emptyTrashTwice = testCase "Calling empty twice does not error" $ do
  tmpDir <- getTestDir
  let testDir = tmpDir </> "e2"
      trashDir = testDir </> ".trash"

  (_, logs1) <- captureSafeRmLogs ["e", "-f", "-t", trashDir]
  assertMatches expectedLogs1 logs1

  (_, logs2) <- captureSafeRmLogs ["e", "-f", "-t", trashDir]
  assertMatches expectedLogs2 logs2
  where
    expectedLogs1 =
      [ Exact "[2020-05-31 12:00:00][functional.emptyTrash][Debug][src/SafeRm.hs:280:4] Trash home: <dir>/e2/.trash",
        Exact "[2020-05-31 12:00:00][functional.emptyTrash][Debug][src/SafeRm.hs:284:8] Trash home does not exist."
      ]
    expectedLogs2 =
      [ Exact "[2020-05-31 12:00:00][functional.emptyTrash][Debug][src/SafeRm.hs:280:4] Trash home: <dir>/e2/.trash",
        Exact "[2020-05-31 12:00:00][functional.emptyTrash][Debug][src/SafeRm.hs:284:8] Trash home does not exist."
      ]
