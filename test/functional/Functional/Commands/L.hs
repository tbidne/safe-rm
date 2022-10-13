-- | Tests for l command.
--
-- @since 0.1
module Functional.Commands.L
  ( tests,
  )
where

import Data.ByteString.Char8 qualified as Char8
import Data.Text qualified as T
import Functional.Prelude
import SafeRm.Exceptions
  ( ExceptionI,
    ExceptionIndex
      ( DuplicateIndexPath,
        ReadIndex,
        TrashIndexSizeMismatch,
        TrashPathNotFound
      ),
  )

-- import Data.ByteString qualified as BS

-- | @since 0.1
tests :: TestTree
tests =
  testGroup
    "List (l)"
    [ emptySucceeds,
      readIndexError,
      indexEntryNonExtantError,
      indexDuplicatesError,
      indexSizeMismatchError
    ]

emptySucceeds :: TestTree
emptySucceeds = testCase "List on empty directory succeeds" $ do
  tmpDir <- getTestDir
  let argList = ["l", "-t", tmpDir </> "l1/.trash"]

  (result, logs) <- captureSafeRmLogs argList
  assertMatches expected result
  assertMatches expectedLogs logs
  where
    expected =
      [ Exact "Entries:      0",
        Exact "Total Files:  0",
        Exact "Size:         0.00B"
      ]
    expectedLogs =
      [ Exact "[2020-05-31 12:00:00][functional.getIndex][Debug][src/SafeRm.hs:193:4] Index path: <dir>/l1/.trash/.index.csv",
        Exact "[2020-05-31 12:00:00][functional.getIndex][Debug][src/SafeRm.hs:197:8] Index does not exist.",
        Exact "[2020-05-31 12:00:00][functional.getMetadata][Debug][src/SafeRm.hs:213:4] Trash home: <dir>/l1/.trash",
        Exact "[2020-05-31 12:00:00][functional.getMetadata][Debug][src/SafeRm.hs:217:8] Trash home does not exist."
      ]

readIndexError :: TestTree
readIndexError = testCase "Read Index Error" $ do
  tmpDir <- getTestDir
  let testDir = tmpDir </> "l2"
      trashDir = testDir </> ".trash"
      argList = ["l", "-t", trashDir]

  -- setup
  clearDirectory testDir
  clearDirectory trashDir
  createFileContents [(trashDir </> ".index.csv", "bad index")]

  (ex, logs) <- captureSafeRmExceptionLogs @(ExceptionI ReadIndex) argList
  assertMatches expected [T.pack $ displayException ex]
  assertMatches expectedLogs logs
  where
    expected =
      [ Outfix
          "Error reading index at"
          "parse error (not enough input) at \"\""
      ]
    expectedLogs =
      [ Exact "[2020-05-31 12:00:00][functional.getIndex][Debug][src/SafeRm.hs:193:4] Index path: <dir>/l2/.trash/.index.csv",
        Exact "[2020-05-31 12:00:00][functional.getIndex.readIndex][Debug][src/SafeRm/Data/Index.hs:103:4] Index path: <dir>/l2/.trash/.index.csv",
        Exact "[2020-05-31 12:00:00][functional.getIndex.readIndex.readIndexWithFold][Error][src/SafeRm/Data/Index.hs:175:8] Error end of stream: parse error (not enough input)",
        Exact "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs:126:8] MkExceptionI (Proxy ExceptionIndex 'ReadIndex) (MkPathI {unPathI = \"<dir>/l2/.trash/.index.csv\"},\"parse error (not enough input) at \\\"\\\"\")"
      ]

indexEntryNonExtantError :: TestTree
indexEntryNonExtantError = testCase "Index Entry Non-Extant Error" $ do
  tmpDir <- getTestDir
  let testDir = tmpDir </> "l3"
      trashDir = testDir </> ".trash"
      argList = ["l", "-t", trashDir]
      badFileLine =
        mconcat
          [ "file,foo,",
            Char8.pack (trashDir </> "foo"),
            ",2022-09-28 02:58:33"
          ]
      badIndex =
        Char8.unlines
          [ "type,name,original,created",
            badFileLine
          ]

  -- setup
  clearDirectory testDir
  clearDirectory trashDir
  createFileContents [(trashDir </> ".index.csv", badIndex)]

  (ex, logs) <- captureSafeRmExceptionLogs @(ExceptionI TrashPathNotFound) argList
  assertMatches expected [T.pack $ displayException ex]
  assertMatches expectedLogs logs
  where
    expected =
      [ Outfix
          "The path 'foo' was not found in the trash directory"
          ( mconcat
              [ "/safe-rm/functional/l3/.trash' despite being listed in the trash index. ",
                "This can be fixed by manually deleting the entry from the ",
                "index or deleting everything (i.e. sr e)."
              ]
          )
      ]
    expectedLogs =
      [ Exact "[2020-05-31 12:00:00][functional.getIndex][Debug][src/SafeRm.hs:193:4] Index path: <dir>/l3/.trash/.index.csv",
        Exact "[2020-05-31 12:00:00][functional.getIndex.readIndex][Debug][src/SafeRm/Data/Index.hs:103:4] Index path: <dir>/l3/.trash/.index.csv",
        Exact "[2020-05-31 12:00:00][functional.getIndex.readIndex.readIndexWithFold][Debug][src/SafeRm/Data/Index.hs:108:8] Found: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"foo\"}, originalPath = MkPathI {unPathI = \"<dir>/l3/.trash/foo\"}, created = MkTimestamp {unTimestamp = 2022-09-28 02:58:33}}",
        Exact "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs:126:8] MkExceptionI (Proxy ExceptionIndex 'TrashPathNotFound) (MkPathI {unPathI = \"<dir>/l3/.trash\"},MkPathI {unPathI = \"foo\"})"
      ]

indexDuplicatesError :: TestTree
indexDuplicatesError = testCase "Index Duplicates Error" $ do
  tmpDir <- getTestDir
  let testDir = tmpDir </> "l4"
      trashDir = testDir </> ".trash"
      argList = ["l", "-t", trashDir]
      dupFile = trashDir </> "foo"
      dupFileLine =
        mconcat
          [ "file,foo,",
            Char8.pack dupFile,
            ",2022-09-28 02:58:33"
          ]
      badIndex =
        Char8.unlines
          [ "type,name,original,created",
            dupFileLine,
            dupFileLine
          ]

  -- setup
  clearDirectory testDir
  clearDirectory trashDir
  createFiles [dupFile]
  createFileContents [(trashDir </> ".index.csv", badIndex)]

  (ex, logs) <- captureSafeRmExceptionLogs @(ExceptionI DuplicateIndexPath) argList
  assertMatches expected [T.pack $ displayException ex]
  assertMatches expectedLogs logs
  where
    expected =
      [ Outfix
          ( mconcat
              [ "Trash paths should be unique, but found multiple entries in ",
                "the trash index"
              ]
          )
          "/safe-rm/functional/l4/.trash/.index.csv' for the following path: foo"
      ]
    expectedLogs =
      [ Exact "[2020-05-31 12:00:00][functional.getIndex][Debug][src/SafeRm.hs:193:4] Index path: <dir>/l4/.trash/.index.csv",
        Exact "[2020-05-31 12:00:00][functional.getIndex.readIndex][Debug][src/SafeRm/Data/Index.hs:103:4] Index path: <dir>/l4/.trash/.index.csv",
        Exact "[2020-05-31 12:00:00][functional.getIndex.readIndex.readIndexWithFold][Debug][src/SafeRm/Data/Index.hs:108:8] Found: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"foo\"}, originalPath = MkPathI {unPathI = \"<dir>/l4/.trash/foo\"}, created = MkTimestamp {unTimestamp = 2022-09-28 02:58:33}}",
        Exact "[2020-05-31 12:00:00][functional.getIndex.readIndex.readIndexWithFold][Debug][src/SafeRm/Data/Index.hs:108:8] Found: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"foo\"}, originalPath = MkPathI {unPathI = \"<dir>/l4/.trash/foo\"}, created = MkTimestamp {unTimestamp = 2022-09-28 02:58:33}}",
        Exact "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs:126:8] MkExceptionI (Proxy ExceptionIndex 'DuplicateIndexPath) (MkPathI {unPathI = \"<dir>/l4/.trash/.index.csv\"},MkPathI {unPathI = \"foo\"})"
      ]

indexSizeMismatchError :: TestTree
indexSizeMismatchError = testCase "Index Size Mismatch Error" $ do
  tmpDir <- getTestDir
  let testDir = tmpDir </> "l5"
      trashDir = testDir </> ".trash"
      argList = ["l", "-t", trashDir]
      file = trashDir </> "foo"
      fileLine =
        mconcat
          [ "file,foo,",
            Char8.pack file,
            ",2022-09-28 02:58:33"
          ]
      badIndex =
        Char8.unlines
          [ "type,name,original,created",
            fileLine
          ]

  -- setup
  clearDirectory testDir
  clearDirectory trashDir
  createFiles [trashDir </> "bar", file]
  createFileContents [(trashDir </> ".index.csv", badIndex)]

  (ex, logs) <- captureSafeRmExceptionLogs @(ExceptionI TrashIndexSizeMismatch) argList
  assertMatches expected [T.pack $ displayException ex]
  assertMatches expectedLogs logs
  where
    expected =
      [ Outfix
          ( mconcat
              [ "Size mismatch between index size (1) and number of ",
                "entries (2) in trash:"
              ]
          )
          "/safe-rm/functional/l5/.trash"
      ]
    expectedLogs =
      [ Exact "[2020-05-31 12:00:00][functional.getIndex][Debug][src/SafeRm.hs:193:4] Index path: <dir>/l5/.trash/.index.csv",
        Exact "[2020-05-31 12:00:00][functional.getIndex.readIndex][Debug][src/SafeRm/Data/Index.hs:103:4] Index path: <dir>/l5/.trash/.index.csv",
        Exact "[2020-05-31 12:00:00][functional.getIndex.readIndex.readIndexWithFold][Debug][src/SafeRm/Data/Index.hs:108:8] Found: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"foo\"}, originalPath = MkPathI {unPathI = \"<dir>/l5/.trash/foo\"}, created = MkTimestamp {unTimestamp = 2022-09-28 02:58:33}}",
        Exact "[2020-05-31 12:00:00][functional.getMetadata][Debug][src/SafeRm.hs:213:4] Trash home: <dir>/l5/.trash",
        Exact "[2020-05-31 12:00:00][functional.getMetadata.toMetadata.readIndex][Debug][src/SafeRm/Data/Index.hs:103:4] Index path: <dir>/l5/.trash/.index.csv",
        Exact "[2020-05-31 12:00:00][functional.getMetadata.toMetadata.readIndex.readIndexWithFold][Debug][src/SafeRm/Data/Index.hs:108:8] Found: MkPathData {pathType = PathTypeFile, fileName = MkPathI {unPathI = \"foo\"}, originalPath = MkPathI {unPathI = \"<dir>/l5/.trash/foo\"}, created = MkTimestamp {unTimestamp = 2022-09-28 02:58:33}}",
        Exact "[2020-05-31 12:00:00][functional.getMetadata.toMetadata][Debug][src/SafeRm/Data/Metadata.hs:96:4] Index size: 1",
        Exact "[2020-05-31 12:00:00][functional.getMetadata.toMetadata][Debug][src/SafeRm/Data/Metadata.hs:98:4] Num entries: 2",
        Exact "[2020-05-31 12:00:00][functional.getMetadata.toMetadata][Debug][src/SafeRm/Data/Metadata.hs:104:4] Num all files: 2",
        Exact "[2020-05-31 12:00:00][functional.getMetadata.toMetadata][Debug][src/SafeRm/Data/Metadata.hs:105:4] Total size: MkSomeSize SB (MkBytes 111.0)",
        Exact "[2020-05-31 12:00:00][functional][Error][src/SafeRm/Runner.hs:126:8] MkExceptionI (Proxy ExceptionIndex 'TrashIndexSizeMismatch) (MkPathI {unPathI = \"<dir>/l5/.trash\"},2,1)"
      ]
