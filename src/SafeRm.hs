{-# LANGUAGE OverloadedLists #-}

-- | Provides functionality for moving a file to a trash location.
--
-- @since 0.1
module SafeRm
  ( -- * Delete
    delete,
    deletePermanently,
    empty,

    -- * Restore
    restore,

    -- * Information
    Metadata (..),
    getIndex,
    getMetadata,
  )
where

import Data.Char qualified as Ch
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import SafeRm.Data.Index (Index (..))
import SafeRm.Data.Index qualified as Index
import SafeRm.Data.Metadata (Metadata (..))
import SafeRm.Data.Metadata qualified as Metadata
import SafeRm.Data.PathData (PathData (..))
import SafeRm.Data.PathData qualified as PathData
import SafeRm.Data.Paths
  ( PathI,
    PathIndex (..),
    _MkPathI,
  )
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Data.Timestamp qualified as Timestamp
import SafeRm.Prelude
import SafeRm.Utils qualified as Utils
import System.Directory qualified as Dir
import System.IO qualified as IO

-- | @delete trash p@ moves path @p@ to the given trash location @trash@ and
-- writes an entry in the trash index. If the trash location is not given,
-- defaults to @~\/.trash@.
--
-- @since 0.1
delete :: Maybe (PathI TrashHome) -> HashSet (PathI OriginalName) -> IO ()
delete mtrash paths = do
  (trashHome, indexPath) <- Paths.getTrashAndIndex mtrash
  Paths.applyPathI (Dir.createDirectoryIfMissing False) trashHome

  deletedPathsRef <- newIORef Map.empty
  currTime <- Timestamp.getCurrentLocalTime

  -- move path to trash
  let delPathsFn = for_ paths $ \fp -> do
        pd <- PathData.toPathData currTime trashHome fp
        PathData.mvOriginalToTrash trashHome pd
        modifyIORef' deletedPathsRef (Map.insert (pd ^. #fileName) pd)

  -- override old index
  delPathsFn `finally` do
    deletedPaths <- readIORef deletedPathsRef
    nonEmpty <-
      Utils.allM1
        [ Paths.applyPathI Dir.doesFileExist indexPath,
          Paths.applyPathI (fmap (> 0) . Dir.getFileSize) indexPath
        ]
    if nonEmpty
      then Index.appendIndex indexPath (MkIndex deletedPaths)
      else Index.writeIndex indexPath (MkIndex deletedPaths)

-- | Permanently deletes the paths from the trash.
--
-- @since 0.1
deletePermanently ::
  Maybe (PathI TrashHome) ->
  Bool ->
  HashSet (PathI TrashName) ->
  IO ()
deletePermanently mtrash force paths = do
  (trashHome, indexPath) <- Paths.getTrashAndIndex mtrash
  index@(MkIndex indexMap) <- Index.readIndex indexPath

  toSafeRmete <- Index.searchIndex False trashHome paths index
  deletedPathsRef <- newIORef Map.empty

  let deleteFn pd = do
        PathData.deletePathData trashHome pd
        modifyIORef' deletedPathsRef (Map.insert (pd ^. #fileName) pd)

  -- move trash paths back to original location
  deletePathsFn <-
    if force
      then do
        pure $ for_ toSafeRmete deleteFn
      else do
        -- NOTE:
        -- - No buffering on input so we can read a single char w/o requiring a
        --   newline to end the input (which then gets passed to getChar, which
        --   interferes with subsequent calls).
        --
        -- - No buffering on output so the "Permanently delete..." string gets
        --   printed w/o the newline.
        IO.hSetBuffering IO.stdin NoBuffering
        IO.hSetBuffering IO.stdout NoBuffering

        pure $ for_ toSafeRmete $ \pd -> do
          let pdStr = (renderStrict . layoutCompact . (line <>) . pretty) pd
          putStrLn $ T.unpack pdStr
          putStr "Permanently delete (y/n)? "
          c <- Ch.toLower <$> IO.getChar
          if
              | c == 'y' -> deleteFn pd *> putStrLn ""
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
getIndex :: Maybe (PathI TrashHome) -> IO Index
getIndex mtrash = do
  indexPath <- view _2 <$> Paths.getTrashAndIndex mtrash
  Paths.applyPathI Dir.doesFileExist indexPath >>= \case
    True -> Index.readIndex indexPath
    False -> pure mempty

-- | Retrieves metadata for the trash directory.
--
-- @since 0.1
getMetadata :: Maybe (PathI TrashHome) -> IO Metadata
getMetadata mtrash = do
  trashData@(trashHome, _) <- Paths.getTrashAndIndex mtrash
  Paths.applyPathI Dir.doesDirectoryExist trashHome >>= \case
    True -> Metadata.getMetadata trashData
    False -> pure mempty

-- | @restore trash p@ restores the trashed path @\<trash\>\/p@ to its original
-- location. If @trash@ is not given then we look in the default location
-- e.g. @~\/.trash@.
--
-- @since 0.1
restore :: Maybe (PathI TrashHome) -> HashSet (PathI TrashName) -> IO ()
restore mtrash paths = do
  (trashHome, indexPath) <- Paths.getTrashAndIndex mtrash
  index@(MkIndex indexMap) <- Index.readIndex indexPath
  toRestore <- Index.searchIndex True trashHome paths index

  restoredPathsRef <- newIORef Map.empty

  -- move trash paths back to original location
  let restorePathsFn = for_ toRestore $ \pd -> do
        PathData.mvTrashToOriginal trashHome pd
        modifyIORef' restoredPathsRef (Map.insert (pd ^. #fileName) pd)

  -- override old index
  restorePathsFn `finally` do
    restoredPaths <- readIORef restoredPathsRef
    Index.writeIndex indexPath (MkIndex $ Map.difference indexMap restoredPaths)

-- | Empties the trash. SafeRmetes the index file.
--
-- @since 0.1
empty :: Maybe (PathI TrashHome) -> IO ()
empty = Paths.getTrashAndIndex >=> Dir.removeDirectoryRecursive . toTrashHome
  where
    toTrashHome = view (_1 % _MkPathI)
