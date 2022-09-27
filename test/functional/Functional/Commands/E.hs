-- | Tests for m command.
--
-- @since 0.1
module Functional.Commands.E
  ( tests,
  )
where

import Functional.Prelude
import Functional.TestArgs (TestArgs (..))

-- | @since 0.1
tests :: IO TestArgs -> TestTree
tests args =
  testGroup
    "Empty (e)"
    [ empty args
    ]

empty :: IO TestArgs -> TestTree
empty args = testCase "Empties trash" $ do
  tmpDir <- view #tmpDir <$> args
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

  runDel delArgList

  -- list output assertions
  resultDel <- captureDel ["l", "-t", trashDir]
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

  let emptyArgList = ["e", "-t", trashDir]
  runDel emptyArgList

  -- list output assertions
  result <- captureDel ["l", "-t", trashDir]
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
      [ Exact "type:      Directory",
        Exact "name:      dir1",
        Outfix "original:" "/del/e1/dir1",
        Prefix "created:",
        Exact "",
        Exact "type:      Directory",
        Exact "name:      dir2",
        Outfix "original:" "/del/e1/dir2",
        Prefix "created:",
        Exact "",
        Exact "type:      File",
        Exact "name:      f1",
        Outfix "original:" "/del/e1/f1",
        Prefix "created:",
        Exact "",
        Exact "type:      File",
        Exact "name:      f2",
        Outfix "original:" "/del/e1/f2",
        Prefix "created:",
        Exact "",
        Exact "type:      File",
        Exact "name:      f3",
        Outfix "original:" "/del/e1/f3",
        Prefix "created:",
        Exact "Entries:      5",
        Exact "Total Files:  4",
        Prefix "Size:"
      ]
    expectedEmpty =
      [ Exact "Entries:      0",
        Exact "Total Files:  0",
        Prefix "Size:"
      ]
