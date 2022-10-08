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
import Data.Text qualified as T
import SafeRm.Data.Index (Index (MkIndex))
import SafeRm.Data.Index qualified as Index
import SafeRm.Data.Metadata (Metadata)
import SafeRm.Data.Metadata qualified as Metadata
import SafeRm.Data.PathData (PathData)
import SafeRm.Data.PathData qualified as PathData
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex (OriginalPath, TrashName),
    _MkPathI,
  )
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Data.Timestamp qualified as Timestamp
import SafeRm.Effects.Logger
import SafeRm.Effects.Logger qualified as Logger
import SafeRm.Effects.Terminal (Terminal (putStr, putStrLn), putTextLn)
import SafeRm.Env
  ( HasTrashHome (getTrashHome),
    HasVerbose (getVerbose),
    getTrashIndex,
    getTrashPaths,
  )
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
  forall env m.
  ( HasTrashHome env,
    HasVerbose env,
    Logger m,
    MonadReader env m,
    MonadUnliftIO m,
    Terminal m
  ) =>
  HashSet (PathI OriginalPath) ->
  m ()
delete paths = addNamespace "delete" $ do
  (trashHome, indexPath) <- asks getTrashPaths
  Logger.logDebug (T.pack $ "Trash home: " <> trashHome ^. _MkPathI)

  verbose <- asks getVerbose
  Paths.applyPathI (Dir.createDirectoryIfMissing False) trashHome

  deletedPathsRef <- newIORef (∅)
  exceptionsRef <- newIORef Nothing
  currTime <- Timestamp.getCurrentLocalTime

  -- move path to trash, saving any exceptions
  addNamespace "deleting" $ for_ paths $ \fp ->
    ( do
        pd <- PathData.toPathData currTime trashHome fp
        Logger.logDebug (showt pd)
        PathData.mvOriginalToTrash trashHome pd
        modifyIORef' deletedPathsRef (Map.insert (pd ^. #fileName) pd)
    )
      `catchAny` \ex -> do
        Logger.logException ex
        modifyIORef' exceptionsRef (Utils.prependMNonEmpty ex)

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

  when verbose $ do
    putStrLn $
      "Successfully deleted the following path(s)\n"
        <> showMapOrigPaths deletedPaths

  Logger.logInfo ("Deleted: " <> showt deletedPaths)

  exceptions <- readIORef exceptionsRef
  Utils.whenJust exceptions $
    -- NOTE: We do not log these exceptions. In general, the only exceptions
    -- we log in the SafeRm API are those that are non-fatal i.e. ones we
    -- catch. Unhandled ones are left to bubble up, where the runner can
    -- cath and log them.
    throwIO . MkExceptionI @SomeExceptions

-- | Permanently deletes the paths from the trash.
--
-- @since 0.1
deletePermanently ::
  forall env m.
  ( HasTrashHome env,
    HasVerbose env,
    Logger m,
    MonadReader env m,
    MonadUnliftIO m,
    Terminal m
  ) =>
  Bool ->
  HashSet (PathI TrashName) ->
  m ()
deletePermanently force paths = addNamespace "deletePermanently" $ do
  (trashHome, indexPath) <- asks getTrashPaths
  verbose <- asks getVerbose
  Logger.logDebug (T.pack $ "Trash home: " <> trashHome ^. _MkPathI)

  index <- Index.readIndex indexPath
  let indexMap = index ^. #unIndex
      (searchExs, toDelete) = Index.searchIndex paths index
  deletedPathsRef <- newIORef (∅)
  exceptionsRef <- newIORef Nothing

  let deleteFn pd =
        ( do
            Logger.logDebug (showt pd)
            PathData.deletePathData trashHome pd
            modifyIORef' deletedPathsRef (Map.insert (pd ^. #fileName) pd)
        )
          `catchAny` \ex -> do
            Logger.logException ex
            modifyIORef' exceptionsRef (Utils.prependMNonEmpty ex)

  -- permanently delete paths
  addNamespace "deleting" $
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
        noBuffering

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
  Index.writeIndex indexPath (MkIndex $ indexMap ∖ deletedPaths)

  when verbose $
    putStrLn $
      "Successfully deleted the following path(s)\n"
        <> showMapTrashPaths deletedPaths

  Logger.logInfo ("Deleted: " <> showt deletedPaths)

  exceptions <- readIORef exceptionsRef
  Utils.whenJust (Utils.concatMNonEmpty searchExs exceptions) $
    throwIO . MkExceptionI @SomeExceptions

-- | Reads the index at either the specified or default location. If the
-- file does not exist, returns empty.
--
-- @since 0.1
getIndex ::
  forall env m.
  ( HasTrashHome env,
    Logger m,
    MonadReader env m,
    MonadIO m
  ) =>
  m Index
getIndex = addNamespace "getIndex" $ do
  indexPath <- asks getTrashIndex
  Logger.logDebug (T.pack $ "Index path: " <> indexPath ^. _MkPathI)
  Paths.applyPathI Dir.doesFileExist indexPath >>= \case
    True -> Index.readIndex indexPath
    False -> do
      Logger.logDebug "Index does not exist."
      pure mempty

-- | Retrieves metadata for the trash directory.
--
-- @since 0.1
getMetadata ::
  forall env m.
  ( HasTrashHome env,
    Logger m,
    MonadReader env m,
    MonadIO m
  ) =>
  m Metadata
getMetadata = addNamespace "getMetadata" $ do
  trashHome <- asks getTrashHome
  Logger.logDebug (T.pack $ "Trash home: " <> trashHome ^. _MkPathI)
  Paths.applyPathI Dir.doesDirectoryExist trashHome >>= \case
    True -> Metadata.getMetadata
    False -> do
      Logger.logDebug "Trash home directory does not exist."
      pure mempty

-- | @restore trash p@ restores the trashed path @\<trash\>\/p@ to its original
-- location. If @trash@ is not given then we look in the default location
-- e.g. @~\/.trash@.
--
-- @since 0.1
restore ::
  forall env m.
  ( HasTrashHome env,
    HasVerbose env,
    Logger m,
    MonadReader env m,
    MonadUnliftIO m,
    Terminal m
  ) =>
  HashSet (PathI TrashName) ->
  m ()
restore paths = addNamespace "restore" $ do
  (trashHome, indexPath) <- asks getTrashPaths
  Logger.logDebug (T.pack $ "Trash home: " <> trashHome ^. _MkPathI)
  verbose <- asks getVerbose
  index <- Index.readIndex indexPath
  let indexMap = index ^. #unIndex
      (searchExs, toRestore) = Index.searchIndex paths index

  restoredPathsRef <- newIORef (∅)
  exceptionsRef <- newIORef Nothing

  -- move trash paths back to original location
  addNamespace "restoring" $ for_ toRestore $ \pd ->
    ( do
        Logger.logDebug (showt pd)
        PathData.mvTrashToOriginal trashHome pd
        modifyIORef' restoredPathsRef (Map.insert (pd ^. #fileName) pd)
    )
      `catchAny` \ex -> do
        Logger.logException ex
        modifyIORef' exceptionsRef (Utils.prependMNonEmpty ex)

  -- override old index
  restoredPaths <- readIORef restoredPathsRef
  Index.writeIndex indexPath (MkIndex $ indexMap ∖ restoredPaths)

  when verbose $
    putStrLn $
      "Successfully restored the following path(s)\n"
        <> showMapOrigPaths restoredPaths

  Logger.logInfo ("Restored: " <> showt restoredPaths)

  exceptions <- readIORef exceptionsRef
  Utils.whenJust (Utils.concatMNonEmpty searchExs exceptions) $
    throwIO . MkExceptionI @SomeExceptions

-- | Empties the trash. Deletes the index file.
--
-- @since 0.1
empty ::
  forall env m.
  ( HasTrashHome env,
    Logger m,
    MonadReader env m,
    MonadIO m,
    Terminal m
  ) =>
  Bool ->
  m ()
empty force = addNamespace "getMetadata" $ do
  MkPathI trashHome <- asks getTrashHome
  Logger.logDebug (T.pack $ "Trash home: " <> trashHome)
  exists <- Dir.doesDirectoryExist trashHome
  if not exists
    then do
      Logger.logDebug "Trash home does not exist."
      putStrLn $ trashHome <> " is empty."
    else
      if force
        then Dir.removeDirectoryRecursive trashHome
        else do
          noBuffering
          putStr "Permanently delete all contents (y/n)? "
          c <- Ch.toLower <$> liftIO IO.getChar
          if
              | c == 'y' -> do
                  Logger.logDebug "Deleting contents."
                  Dir.removeDirectoryRecursive trashHome
                  putStrLn ""
              | c == 'n' -> do
                  Logger.logDebug "Not deleting contents."
                  putStrLn ""
              | otherwise -> putStrLn ("\nUnrecognized: " <> [c])

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

noBuffering :: MonadIO m => m ()
noBuffering = liftIO $ buffOff IO.stdin *> buffOff IO.stdout
  where
    buffOff h = IO.hSetBuffering h NoBuffering
