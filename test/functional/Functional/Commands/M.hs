-- | Tests for m command.
--
-- @since 0.1
module Functional.Commands.M
  ( tests,
  )
where

import Functional.Prelude

-- | @since 0.1
tests :: IO FilePath -> TestTree
tests args =
  testGroup
    "Metadata (m)"
    [ metadata args
    ]

metadata :: IO FilePath -> TestTree
metadata args = goldenVsStringDiff "Prints metadata" diff gpath $ do
  -- metadata = goldenVsString "Prints metadata" gpath $ do
  tmpDir <- args
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

  runSafeRm tmpDir delArgList

  -- list output assertions
  delResult <- captureSafeRm tmpDir "LIST" ["l", "-t", trashDir]

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
  (metadataResult, logs) <- captureSafeRmLogs tmpDir "METADATA" metaArgList

  -- assert nothing changed
  assertFilesExist
    ( (trashDir </>)
        <$> [".index.csv", "f1", "f2", "f3", "dir2/dir3/foo"]
    )
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesExist ((trashDir </>) <$> ["", "dir1", "dir2", "dir2/dir3"])

  pure $ capturedToBs [delResult, metadataResult, logs]
  where
    gpath = goldenPath </> "metadata.golden"

goldenPath :: FilePath
goldenPath = "test/functional/Functional/Commands/M"
