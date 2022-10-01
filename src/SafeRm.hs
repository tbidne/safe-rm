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
    getIndex,
    getMetadata,
  )
where

import Data.Char qualified as Ch
import Data.HashMap.Strict qualified as Map
import SafeRm.Data.Index (Index (MkIndex))
import SafeRm.Data.Index qualified as Index
import SafeRm.Data.Metadata (Metadata)
import SafeRm.Data.Metadata qualified as Metadata
import SafeRm.Data.PathData qualified as PathData
import SafeRm.Data.Paths
  ( PathI,
    PathIndex (OriginalPath, TrashHome, TrashName),
    _MkPathI,
  )
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Data.Timestamp qualified as Timestamp
import SafeRm.Effects.Terminal (Terminal (putStr, putStrLn), putTextLn)
import SafeRm.Prelude
import SafeRm.Utils qualified as Utils
import System.IO qualified as IO
import UnliftIO.Directory qualified as Dir

-- | @delete trash p@ moves path @p@ to the given trash location @trash@ and
-- writes an entry in the trash index. If the trash location is not given,
-- defaults to @~\/.trash@.
--
-- @since 0.1
delete ::
  forall m.
  MonadUnliftIO m =>
  Maybe (PathI TrashHome) ->
  HashSet (PathI OriginalPath) ->
  m ()
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
  ( MonadUnliftIO m,
    Terminal m
  ) =>
  Maybe (PathI TrashHome) ->
  Bool ->
  HashSet (PathI TrashName) ->
  m ()
deletePermanently mtrash force paths = do
  (trashHome, indexPath) <- Paths.getTrashAndIndex mtrash
  index@(MkIndex indexMap) <- Index.readIndex indexPath

  toDelete <- Index.searchIndex False trashHome paths index
  deletedPathsRef <- newIORef Map.empty

  let deleteFn pd = do
        PathData.deletePathData trashHome pd
        modifyIORef' deletedPathsRef (Map.insert (pd ^. #fileName) pd)

  -- permanently delete paths
  deletePathsFn <-
    if force
      then pure $ for_ toDelete deleteFn
      else do
        -- NOTE:
        -- - No buffering on input so we can read a single char w/o requiring a
        --   newline to end the input (which then gets passed to getChar, which
        --   interferes with subsequent calls).
        --
        -- - No buffering on output so the "Permanently delete..." string gets
        --   printed w/o the newline.
        liftIO $ do
          IO.hSetBuffering IO.stdin NoBuffering
          IO.hSetBuffering IO.stdout NoBuffering

        pure $ for_ toDelete $ \pd -> do
          let pdStr = (renderStrict . layoutCompact . (line <>) . pretty) pd
          putTextLn pdStr
          putStr "Permanently delete (y/n)? "
          c <- Ch.toLower <$> liftIO IO.getChar
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
getIndex ::
  MonadIO m =>
  Maybe (PathI TrashHome) ->
  m Index
getIndex mtrash = do
  indexPath <- view _2 <$> Paths.getTrashAndIndex mtrash
  Paths.applyPathI Dir.doesFileExist indexPath >>= \case
    True -> Index.readIndex indexPath
    False -> pure mempty

-- | Retrieves metadata for the trash directory.
--
-- @since 0.1
getMetadata ::
  MonadIO m =>
  Maybe (PathI TrashHome) ->
  m Metadata
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
restore ::
  MonadUnliftIO m =>
  Maybe (PathI TrashHome) ->
  HashSet (PathI TrashName) ->
  m ()
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

-- | Empties the trash. Deletes the index file.
--
-- @since 0.1
empty ::
  MonadIO m =>
  Maybe (PathI TrashHome) ->
  m ()
empty = Paths.getTrashAndIndex >=> Dir.removeDirectoryRecursive . toTrashHome
  where
    toTrashHome = view (_1 % _MkPathI)
