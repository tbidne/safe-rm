-- | Tests for m command.
--
-- @since 0.1
module Functional.Commands.M
  ( tests,
  )
where

import Functional.Prelude

-- | @since 0.1
tests :: TestTree
tests =
  testGroup
    "Metadata (m)"
    [ metadata
    ]

metadata :: TestTree
metadata = testCase "Prints metadata" $ do
  tmpDir <- getTestDir
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

  runSafeRm delArgList

  -- list output assertions
  delResult <- captureSafeRm ["l", "-t", trashDir]
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
  (metadataResult, logs) <- captureSafeRmLogs metaArgList

  -- list output assertions
  assertMatches expectedMetadata metadataResult
  assertMatches expectedLogs logs

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
      [ Exact "Type:      Directory",
        Exact "Name:      dir1",
        Outfix "Original:" "/safe-rm/functional/m1/dir1",
        Prefix "Created:",
        Exact "",
        Exact "Type:      Directory",
        Exact "Name:      dir2",
        Outfix "Original:" "/safe-rm/functional/m1/dir2",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f1",
        Outfix "Original:" "/safe-rm/functional/m1/f1",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f2",
        Outfix "Original:" "/safe-rm/functional/m1/f2",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f3",
        Outfix "Original:" "/safe-rm/functional/m1/f3",
        Prefix "Created:",
        Exact "Entries:      5",
        Exact "Total Files:  4",
        Prefix "Size:"
      ]
    expectedMetadata =
      [ Exact "Entries:      5",
        Exact "Total Files:  4",
        Prefix "Size:"
      ]
    expectedLogs =
      [ Exact "[2020-05-31 12:00:00][functional.getMetadata][Debug][src/SafeRm.hs:213:4] Trash home: <dir>/m1/.trash",
        Exact "[2020-05-31 12:00:00][functional.getMetadata.toMetadata.readIndex][Debug][src/SafeRm/Data/Index.hs:103:4] Index path: <dir>/m1/.trash/.index.csv",
        Exact "[2020-05-31 12:00:00][functional.getMetadata.toMetadata.readIndex.readIndexWithFold][Debug][src/SafeRm/Data/Index.hs:108:8] Found: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f1\"}, originalPath = MkPathI {unPathI = \"<dir>/m1/f1\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.getMetadata.toMetadata.readIndex.readIndexWithFold][Debug][src/SafeRm/Data/Index.hs:108:8] Found: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f3\"}, originalPath = MkPathI {unPathI = \"<dir>/m1/f3\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.getMetadata.toMetadata.readIndex.readIndexWithFold][Debug][src/SafeRm/Data/Index.hs:108:8] Found: MkPathData {pathType = PathTypeDirectory, fileName = MkPathI {unPathI = \"dir1\"}, originalPath = MkPathI {unPathI = \"<dir>/m1/dir1\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.getMetadata.toMetadata.readIndex.readIndexWithFold][Debug][src/SafeRm/Data/Index.hs:108:8] Found: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"f2\"}, originalPath = MkPathI {unPathI = \"<dir>/m1/f2\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.getMetadata.toMetadata.readIndex.readIndexWithFold][Debug][src/SafeRm/Data/Index.hs:108:8] Found: MkPathData {pathType = PathTypeDirectory, fileName = MkPathI {unPathI = \"dir2\"}, originalPath = MkPathI {unPathI = \"<dir>/m1/dir2\"}, created = MkTimestamp {unTimestamp = 2020-05-31 12:00:00}}",
        Exact "[2020-05-31 12:00:00][functional.getMetadata.toMetadata][Debug][src/SafeRm/Data/Metadata.hs:96:4] Index size: 5",
        Exact "[2020-05-31 12:00:00][functional.getMetadata.toMetadata][Debug][src/SafeRm/Data/Metadata.hs:98:4] Num entries: 5",
        Exact "[2020-05-31 12:00:00][functional.getMetadata.toMetadata][Debug][src/SafeRm/Data/Metadata.hs:104:4] Num all files: 4",
        Exact "[2020-05-31 12:00:00][functional.getMetadata.toMetadata][Debug][src/SafeRm/Data/Metadata.hs:105:4] Total size: MkSomeSize SB (MkBytes 426.0)"
      ]
