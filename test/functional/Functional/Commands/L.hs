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
import Functional.TestArgs (TestArgs (..))
import SafeRm.Exceptions

-- import Data.ByteString qualified as BS

-- | @since 0.1
tests :: IO TestArgs -> TestTree
tests args =
  testGroup
    "List (l)"
    [ emptySucceeds args,
      readIndexError args,
      indexEntryNonExtantError args,
      indexDuplicatesError args,
      indexSizeMismatchError args
    ]

emptySucceeds :: IO TestArgs -> TestTree
emptySucceeds args = testCase "List on empty directory succeeds" $ do
  tmpDir <- view #tmpDir <$> args
  let argList = ["l", "-t", tmpDir </> "l1/.trash"]

  result <- captureDel argList
  assertMatches expected result
  where
    expected =
      [ Exact "Entries:      0",
        Exact "Total Files:  0",
        Exact "Size:         0.00B"
      ]

readIndexError :: IO TestArgs -> TestTree
readIndexError args = testCase "Read Index Error" $ do
  tmpDir <- view #tmpDir <$> args
  let testDir = tmpDir </> "l2"
      trashDir = testDir </> ".trash"
      argList = ["l", "-t", trashDir]

  -- setup
  clearDirectory testDir
  clearDirectory trashDir
  createFileContents [(trashDir </> ".index.csv", "bad index")]

  -- assert exception
  result <-
    (runSafeRm argList $> Nothing)
      `catch` \(e :: ExceptionI ReadIndex) -> pure (Just e)
  case result of
    Nothing -> assertFailure "Expected exception"
    Just ex -> assertMatches expected [T.pack $ displayException ex]
  where
    expected =
      [ Outfix
          "Error reading index at"
          "parse error (not enough input) at \"\""
      ]

indexEntryNonExtantError :: IO TestArgs -> TestTree
indexEntryNonExtantError args = testCase "Index Entry Non-Extant Error" $ do
  tmpDir <- view #tmpDir <$> args
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

  -- assert exception
  result <-
    (runSafeRm argList $> Nothing)
      `catch` \(e :: ExceptionI TrashPathNotFound) -> pure (Just e)
  case result of
    Nothing -> assertFailure "Expected exception"
    Just ex -> assertMatches expected [T.pack $ displayException ex]
  where
    expected =
      [ Outfix
          "The path 'foo' was not found in the trash directory"
          "/safe-rm/l3/.trash' despite being listed in the trash index."
      ]

indexDuplicatesError :: IO TestArgs -> TestTree
indexDuplicatesError args = testCase "Index Duplicates Error" $ do
  tmpDir <- view #tmpDir <$> args
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

  -- assert exception
  result <-
    (runSafeRm argList $> Nothing)
      `catch` \(e :: ExceptionI DuplicateIndexPath) -> pure (Just e)
  case result of
    Nothing -> assertFailure "Expected exception"
    Just ex -> assertMatches expected [T.pack $ displayException ex]
  where
    expected =
      [ Outfix
          ( mconcat
              [ "Trash paths should be unique, but found multiple entries in ",
                "the trash index"
              ]
          )
          "/safe-rm/l4/.trash/.index.csv' for the following path: foo"
      ]

indexSizeMismatchError :: IO TestArgs -> TestTree
indexSizeMismatchError args = testCase "Index Size Mismatch Error" $ do
  tmpDir <- view #tmpDir <$> args
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

  -- assert exception
  result <-
    (runSafeRm argList $> Nothing)
      `catch` \(e :: ExceptionI TrashIndexSizeMismatch) -> pure (Just e)
  case result of
    Nothing -> assertFailure "Expected exception"
    Just ex -> assertMatches expected [T.pack $ displayException ex]
  where
    expected =
      [ Outfix
          ( mconcat
              [ "Size mismatch between index size (1) and number of ",
                "entries (2) in trash:"
              ]
          )
          "/safe-rm/l5/.trash"
      ]
