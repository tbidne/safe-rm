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
import Del.Data.Index (Index (..))
import Del.Data.Index qualified as Index
import Del.Data.PathData (PathData (..))
import Del.Data.PathData qualified as PathData
import Del.Data.PathType qualified as PathType
import Del.Data.Statistics (Statistics (..))
import Del.Data.Statistics qualified as Stats
import Del.Data.Timestamp qualified as Timestamp
import Del.Prelude
import Del.Utils qualified as Utils
import System.Directory qualified as Dir
import System.IO qualified as IO

-- | @del trash p@ moves path @p@ to the given trash location @trash@ and
-- writes an entry in the trash index. If the trash location is not given,
-- defaults to @~\/.trash@.
--
-- @since 0.1
del :: Maybe FilePath -> HashSet FilePath -> IO ()
del mtrash paths = do
  (trashHome, indexPath) <- Utils.getTrashAndIndex mtrash
  Dir.createDirectoryIfMissing False trashHome

  deletedPathsRef <- newIORef Map.empty
  currTime <- Timestamp.getCurrentLocalTime

  -- move path to trash
  let delPathsFn = for_ paths $ \fp -> do
        pd <- PathData.toPathData currTime trashHome fp
        PathData.mvToTrash trashHome pd
        modifyIORef' deletedPathsRef (Map.insert (pd ^. #fileName) pd)

  -- override old index
  delPathsFn `finally` do
    deletedPaths <- readIORef deletedPathsRef
    nonEmpty <-
      Utils.allM1
        [ Dir.doesFileExist indexPath,
          (> 0) <$> Dir.getFileSize indexPath
        ]
    if nonEmpty
      then Index.appendIndex indexPath (MkIndex deletedPaths)
      else Index.writeIndex indexPath (MkIndex deletedPaths)

-- | Permanently deletes the paths from the trash.
--
-- @since 0.1
permDel :: Maybe FilePath -> HashSet FilePath -> IO ()
permDel mtrash paths = do
  (trashHome, indexPath) <- Utils.getTrashAndIndex mtrash
  index@(MkIndex indexMap) <- Index.readIndex indexPath

  -- NOTE:
  -- - No buffering on input so we can read a single char w/o requiring a
  --   newline to end the input (which then gets passed to getChar, which
  --   interferes with subsequent calls).
  --
  -- - No buffering on output so the "Permanently delete..." string gets
  --   printed w/o the newline.
  IO.hSetBuffering IO.stdin NoBuffering
  IO.hSetBuffering IO.stdout NoBuffering

  toDelete <- Index.searchIndex False trashHome paths index

  deletedPathsRef <- newIORef Map.empty

  -- move trash paths back to original location
  let deletePathsFn = for_ toDelete $ \pd -> do
        let pdStr = (renderStrict . layoutCompact . (line <>) . pretty) pd
        putStrLn $ T.unpack pdStr
        -- TODO: add a "-f" override
        putStr "Permanently delete (y/n)? "
        c <- Ch.toLower <$> IO.getChar
        if
            | c == 'y' -> do
                Dir.removePathForcibly (trashHome </> (pd ^. #fileName))
                modifyIORef' deletedPathsRef (Map.insert (pd ^. #fileName) pd)
                putStrLn ""
            | c == 'n' -> putStrLn ""
            | otherwise -> putStrLn ("\nUnrecognized: " <> [c])

  -- override old index
  deletePathsFn `finally` do
    deletedPaths <- readIORef deletedPathsRef
    Index.writeIndex indexPath (MkIndex $ Map.difference indexMap deletedPaths)

-- | Reads the index at either the specified or default location. If the
-- file does not exist, returns empty.
--
-- @since 0.1
getIndex :: Maybe FilePath -> IO Index
getIndex mtrash = do
  indexPath <- view _2 <$> Utils.getTrashAndIndex mtrash
  Dir.doesFileExist indexPath >>= \case
    True -> Index.readIndex indexPath
    False -> pure mempty

-- | Retrieves statistics for the trash directory.
--
-- @since 0.1
getStatistics :: Maybe FilePath -> IO Statistics
getStatistics = Utils.getTrashAndIndex >=> Stats.getStats . view _1

-- | @restore trash p@ restores the trashed path @\<trash\>\/p@ to its original
-- location. If @trash@ is not given then we look in the default location
-- e.g. @~\/.trash@.
--
-- @since 0.1
restore :: Maybe FilePath -> HashSet FilePath -> IO ()
restore mtrash paths = do
  (trashHome, indexPath) <- Utils.getTrashAndIndex mtrash
  index@(MkIndex indexMap) <- Index.readIndex indexPath
  toRestore <- Index.searchIndex True trashHome paths index

  restoredPathsRef <- newIORef Map.empty

  -- move trash paths back to original location
  let restorePathsFn = for_ toRestore $ \pd -> do
        PathType.pathTypeToRenameFn
          (pd ^. #pathType)
          (trashHome </> pd ^. #fileName)
          (pd ^. #originalPath)
        modifyIORef' restoredPathsRef (Map.insert (pd ^. #fileName) pd)

  -- override old index
  restorePathsFn `finally` do
    restoredPaths <- readIORef restoredPathsRef
    Index.writeIndex indexPath (MkIndex $ Map.difference indexMap restoredPaths)

-- | Empties the trash. Deletes the index file.
--
-- @since 0.1
empty :: Maybe FilePath -> IO ()
empty = Utils.getTrashAndIndex >=> Dir.removeDirectoryRecursive . view _1
