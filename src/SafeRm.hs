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
import SafeRm.Data.PathData (PathData)
import SafeRm.Data.PathData qualified as PathData
import SafeRm.Data.Paths
  ( PathI,
    PathIndex (OriginalPath, TrashHome, TrashName),
    _MkPathI,
  )
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Data.Timestamp qualified as Timestamp
import SafeRm.Effects.Terminal (Terminal (putStr, putStrLn), putTextLn)
import SafeRm.Exceptions
  ( ExceptionI (MkExceptionI),
    ExceptionIndex (SomeExceptions),
  )
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
  (MonadUnliftIO m, Terminal m) =>
  Bool ->
  Maybe (PathI TrashHome) ->
  HashSet (PathI OriginalPath) ->
  m ()
delete verbose mtrash paths = do
  (trashHome, indexPath) <- Paths.getTrashAndIndex mtrash
  Paths.applyPathI (Dir.createDirectoryIfMissing False) trashHome

  deletedPathsRef <- newIORef Map.empty
  exceptionsRef <- newIORef Nothing
  currTime <- Timestamp.getCurrentLocalTime

  -- move path to trash, saving any exceptions
  for_ paths $ \fp ->
    ( do
        pd <- PathData.toPathData currTime trashHome fp
        PathData.mvOriginalToTrash trashHome pd
        modifyIORef' deletedPathsRef (Map.insert (pd ^. #fileName) pd)
    )
      `catchAny` \ex -> modifyIORef' exceptionsRef (Utils.prependMNonEmpty ex)

  -- override old index
  deletedPaths <- readIORef deletedPathsRef
  nonEmpty <-
    Utils.allM1
      [ Paths.applyPathI Dir.doesFileExist indexPath,
        Paths.applyPathI (fmap (> 0) . Dir.getFileSize) indexPath
      ]
  if nonEmpty
    then Index.appendIndex indexPath (MkIndex deletedPaths)
    else Index.writeIndex indexPath (MkIndex deletedPaths)

  when verbose $
    putStrLn $
      "Successfully deleted the following path(s)\n"
        <> showMapOrigPaths deletedPaths

  exceptions <- readIORef exceptionsRef
  Utils.whenJust exceptions $ throwIO . MkExceptionI @SomeExceptions

-- | Permanently deletes the paths from the trash.
--
-- @since 0.1
deletePermanently ::
  ( MonadUnliftIO m,
    Terminal m
  ) =>
  Bool ->
  Maybe (PathI TrashHome) ->
  Bool ->
  HashSet (PathI TrashName) ->
  m ()
deletePermanently verbose mtrash force paths = do
  (trashHome, indexPath) <- Paths.getTrashAndIndex mtrash
  index <- Index.readIndex indexPath
  let indexMap = index ^. #unIndex
      (searchExs, toDelete) = Index.searchIndex paths index
  deletedPathsRef <- newIORef Map.empty
  exceptionsRef <- newIORef Nothing

  let deleteFn pd =
        ( do
            PathData.deletePathData trashHome pd
            modifyIORef' deletedPathsRef (Map.insert (pd ^. #fileName) pd)
        )
          `catchAny` \ex -> modifyIORef' exceptionsRef (Utils.prependMNonEmpty ex)

  -- permanently delete paths
  if force
    then for_ toDelete deleteFn
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

      for_ toDelete $ \pd -> do
        let pdStr = (renderStrict . layoutCompact . (line <>) . pretty) pd
        putTextLn pdStr
        putStr "Permanently delete (y/n)? "
        c <- Ch.toLower <$> liftIO IO.getChar
        if
            | c == 'y' -> deleteFn pd *> putStrLn ""
            | c == 'n' -> putStrLn ""
            | otherwise -> putStrLn ("\nUnrecognized: " <> [c])

  -- override old index
  deletedPaths <- readIORef deletedPathsRef
  Index.writeIndex indexPath (MkIndex $ Map.difference indexMap deletedPaths)

  when verbose $
    putStrLn $
      "Successfully deleted the following path(s)\n"
        <> showMapTrashPaths deletedPaths

  exceptions <- readIORef exceptionsRef
  Utils.whenJust (Utils.concatMNonEmpty searchExs exceptions) $
    throwIO . MkExceptionI @SomeExceptions

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
  (MonadUnliftIO m, Terminal m) =>
  Bool ->
  Maybe (PathI TrashHome) ->
  HashSet (PathI TrashName) ->
  m ()
restore verbose mtrash paths = do
  (trashHome, indexPath) <- Paths.getTrashAndIndex mtrash
  index <- Index.readIndex indexPath
  let indexMap = index ^. #unIndex
      (searchExs, toRestore) = Index.searchIndex paths index

  restoredPathsRef <- newIORef Map.empty
  exceptionsRef <- newIORef Nothing

  -- move trash paths back to original location
  for_ toRestore $ \pd ->
    ( do
        PathData.mvTrashToOriginal trashHome pd
        modifyIORef' restoredPathsRef (Map.insert (pd ^. #fileName) pd)
    )
      `catchAny` \ex -> modifyIORef' exceptionsRef (Utils.prependMNonEmpty ex)

  -- override old index
  restoredPaths <- readIORef restoredPathsRef
  Index.writeIndex indexPath (MkIndex $ Map.difference indexMap restoredPaths)

  when verbose $
    putStrLn $
      "Successfully restored the following path(s)\n"
        <> showMapOrigPaths restoredPaths

  exceptions <- readIORef exceptionsRef
  Utils.whenJust (Utils.concatMNonEmpty searchExs exceptions) $
    throwIO . MkExceptionI @SomeExceptions

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

showMapTrashPaths :: HashMap (PathI 'TrashName) PathData -> String
showMapTrashPaths = showMapElems #fileName

showMapOrigPaths :: HashMap (PathI 'TrashName) PathData -> String
showMapOrigPaths = showMapElems #originalPath

showMapElems :: Lens' PathData (PathI i) -> HashMap (PathI TrashName) PathData -> String
showMapElems toPathI =
  foldl' foldStrs ""
    . fmap (view (toPathI % _MkPathI))
    . Map.elems
  where
    foldStrs acc s = ("- " <> s <> "\n") <> acc
