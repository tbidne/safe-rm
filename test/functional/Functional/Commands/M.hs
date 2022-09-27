-- | Tests for m command.
--
-- @since 0.1
module Functional.Commands.M
  ( tests,
  )
where

import Functional.Prelude
import Functional.TestArgs (TestArgs (..))

-- | @since 0.1
tests :: IO TestArgs -> TestTree
tests args =
  testGroup
    "Metadata (m)"
    [ metadata args
    ]

metadata :: IO TestArgs -> TestTree
metadata args = testCase "Prints metadata" $ do
  tmpDir <- view #tmpDir <$> args
  let testDir = tmpDir </> "m1"
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
  delResult <- captureDel ["l", "-t", trashDir]
  assertMatches expectedDel delResult

  -- file assertions
  assertFilesExist
    ( (trashDir </>)
        <$> [".index.csv", "f1", "f2", "f3", "dir2/dir3/foo"]
    )
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesExist ((trashDir </>) <$> ["", "dir1", "dir2", "dir2/dir3"])

  -- METADATA

  let metaArgList = ["m", "-t", trashDir]
  metadataResult <- captureDel metaArgList

  -- list output assertions
  assertMatches expectedMetadata metadataResult

  -- assert nothing changed
  assertFilesExist
    ( (trashDir </>)
        <$> [".index.csv", "f1", "f2", "f3", "dir2/dir3/foo"]
    )
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesExist ((trashDir </>) <$> ["", "dir1", "dir2", "dir2/dir3"])
  where
    expectedDel =
      [ Exact "type:      Directory",
        Exact "name:      dir1",
        Outfix "original:" "/del/m1/dir1",
        Prefix "created:",
        Exact "",
        Exact "type:      Directory",
        Exact "name:      dir2",
        Outfix "original:" "/del/m1/dir2",
        Prefix "created:",
        Exact "",
        Exact "type:      File",
        Exact "name:      f1",
        Outfix "original:" "/del/m1/f1",
        Prefix "created:",
        Exact "",
        Exact "type:      File",
        Exact "name:      f2",
        Outfix "original:" "/del/m1/f2",
        Prefix "created:",
        Exact "",
        Exact "type:      File",
        Exact "name:      f3",
        Outfix "original:" "/del/m1/f3",
        Prefix "created:",
        Exact "Entries:      5",
        Exact "Total Files:  4",
        Prefix "Size:"
      ]
    expectedMetadata =
      [ Exact "Entries:      5",
        Exact "Total Files:  4",
        Prefix "Size:"
      ]
