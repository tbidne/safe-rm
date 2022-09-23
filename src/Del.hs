{-# LANGUAGE OverloadedLists #-}

-- | Provides functionality for moving a file to a trash location.
--
-- @since 0.1
module Del
  ( -- * Deletion
    del,
    permDel,
    empty,

    -- * Restore
    restore,

    -- * Information
    Statistics (..),
    getIndex,
    getStatistics,
  )
where

import Data.Char qualified as Ch
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Del.Internal qualified as I
import Del.Prelude
import Del.Types (Index (..), PathData (..), Statistics (..))
import System.Directory qualified as Dir
import System.IO qualified as IO

-- | @del trash p@ moves path @p@ to the given trash location @trash@ and
-- writes an entry in the trash index. If the trash location is not given,
-- defaults to @~\/.trash@.
--
-- @since 0.1
del :: Maybe FilePath -> HashSet FilePath -> IO ()
del mtrash paths = do
  trashHome <- I.trashOrDefault mtrash
  let indexPath = I.getIndexPath trashHome
  Dir.createDirectoryIfMissing False trashHome

  deletedPathsRef <- newIORef Map.empty

  -- move path to trash
  let delPathsFn = for_ paths $ \fp -> do
        pd <- I.toPathData trashHome fp
        I.mvToTrash pd
        modifyIORef' deletedPathsRef (Map.insert (pd ^. #trashPath) pd)

  -- override old index
  delPathsFn `finally` do
    -- TODO: mask all exceptions and enforce no-throw
    deletedPaths <- readIORef deletedPathsRef
    nonEmpty <-
      I.allM1
        [ Dir.doesFileExist indexPath,
          (> 0) <$> Dir.getFileSize indexPath
        ]
    if nonEmpty
      then I.appendIndex indexPath (MkIndex deletedPaths)
      else I.writeIndex indexPath (MkIndex deletedPaths)

-- | Permanently deletes the paths from the trash.
--
-- @since 0.1
permDel :: Maybe FilePath -> HashSet FilePath -> IO ()
permDel mtrash paths = do
  trashHome <- I.trashOrDefault mtrash
  let indexPath = I.getIndexPath trashHome
  index@(MkIndex indexMap) <- I.readIndex indexPath

  -- NOTE: No buffering on input so we can read a single char w/o requiring a
  -- newline to end the input (which then gets passed to getChar, which
  -- interferes with subsequent calls).
  IO.hSetBuffering IO.stdin NoBuffering

  -- NOTE: No buffering on output so the "Permanently delete..." string gets
  -- printed w/o the newline.
  IO.hSetBuffering IO.stdout NoBuffering

  toDelete <- I.searchIndexForPermDel trashHome paths index

  deletedPathsRef <- newIORef Map.empty

  -- move trash paths back to original location
  let deletePathsFn = for_ toDelete $ \pd -> do
        let pdStr = (renderStrict . layoutCompact . (line <>) . pretty) pd
        putStrLn $ T.unpack pdStr
        putStr "Permanently delete (y/n)? "
        c <- Ch.toLower <$> IO.getChar
        if
            | c == 'y' -> do
                Dir.removePathForcibly (pd ^. #trashPath)
                modifyIORef' deletedPathsRef (Map.insert (pd ^. #trashPath) pd)
                putStrLn ""
            | c == 'n' -> putStrLn ""
            | otherwise -> putStrLn ("\nUnrecognized: " <> [c])

  -- override old index
  deletePathsFn `finally` do
    -- TODO: mask all exceptions and enforce no-throw
    deletedPaths <- readIORef deletedPathsRef
    I.writeIndex indexPath (MkIndex $ Map.difference indexMap deletedPaths)

-- | Reads the index at either the specified or default location. If the
-- file does not exist, returns empty.
--
-- @since 0.1
getIndex :: Maybe FilePath -> IO Index
getIndex mtrash = do
  trashHome <- I.trashOrDefault mtrash
  let indexPath = I.getIndexPath trashHome
  Dir.doesFileExist indexPath >>= \case
    True -> I.readIndex indexPath
    False -> pure mempty

-- | Retrieves statistics for the trash directory.
--
-- @since 0.1
getStatistics :: Maybe FilePath -> IO Statistics
getStatistics = I.trashOrDefault >=> I.getStats

-- | @restore trash p@ restores the trashed path @\<trash\>\/p@ to its original
-- location. If @trash@ is not given then we look in the default location
-- e.g. @~\/.trash@.
--
-- @since 0.1
restore :: Maybe FilePath -> HashSet FilePath -> IO ()
restore mtrash paths = do
  trashHome <- I.trashOrDefault mtrash
  let indexPath = I.getIndexPath trashHome
  index@(MkIndex indexMap) <- I.readIndex indexPath
  toRestore <- I.searchIndexForRestore trashHome paths index

  restoredPathsRef <- newIORef Map.empty

  -- move trash paths back to original location
  let restorePathsFn = for_ toRestore $ \pd -> do
        I.pathTypeToRenameFn
          (pd ^. #pathType)
          (pd ^. #trashPath)
          (pd ^. #originalPath)
        modifyIORef' restoredPathsRef (Map.insert (pd ^. #trashPath) pd)

  -- override old index
  restorePathsFn `finally` do
    -- TODO: mask all exceptions and enforce no-throw
    restoredPaths <- readIORef restoredPathsRef
    I.writeIndex indexPath (MkIndex $ Map.difference indexMap restoredPaths)

-- | Empties the trash. Deletes the index file.
--
-- @since 0.1
empty :: Maybe FilePath -> IO ()
empty = I.trashOrDefault >=> Dir.removeDirectoryRecursive
