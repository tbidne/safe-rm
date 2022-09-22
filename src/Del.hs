{-# LANGUAGE CPP #-}
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

import Control.Exception (finally)
import Control.Monad ((>=>))
import Data.Char qualified as Ch
import Data.Foldable (for_)
import Data.HashMap.Strict qualified as Map
import Data.HashSet (HashSet)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Del.Internal
  ( allM1,
    appendIndex,
    getIndexPath,
    getStats,
    mvToTrash,
    pathTypeToRenameFn,
    readIndex,
    searchIndexForPermDel,
    searchIndexForRestore,
    toPathData,
    trashOrDefault,
    writeIndex,
  )
#if !MIN_VERSION_prettyprinter(1, 7, 1)
import Data.Text.Prettyprint.Doc (Pretty (pretty), layoutCompact)
import Data.Text.Prettyprint.Doc qualified as Pretty
import Data.Text.Prettyprint.Render.String (renderString)
#else
import Prettyprinter (Pretty (pretty), layoutCompact)
import Prettyprinter qualified as Pretty
import Prettyprinter.Render.String (renderString)
#endif
import Del.Types (Index (..), PathData (..), Statistics (..))
import Optics.Core ((^.))
import System.Directory qualified as Dir
import System.IO qualified as IO

-- | @del trash p@ moves path @p@ to the given trash location @trash@ and
-- writes an entry in the trash index. If the trash location is not given,
-- defaults to @~\/.trash@.
--
-- @since 0.1
del :: Maybe FilePath -> HashSet FilePath -> IO ()
del mtrash paths = do
  trashHome <- trashOrDefault mtrash
  let indexPath = getIndexPath trashHome
  Dir.createDirectoryIfMissing False trashHome

  deletedPathsRef <- newIORef Map.empty

  -- move path to trash
  let delPathsFn = for_ paths $ \fp -> do
        pd <- toPathData trashHome fp
        mvToTrash pd
        modifyIORef' deletedPathsRef (Map.insert (pd ^. #trashPath) pd)

  -- override old index
  delPathsFn `finally` do
    -- TODO: mask all exceptions and enforce no-throw
    deletedPaths <- readIORef deletedPathsRef
    nonEmpty <-
      allM1
        [ Dir.doesFileExist indexPath,
          (> 0) <$> Dir.getFileSize indexPath
        ]
    if nonEmpty
      then appendIndex indexPath (MkIndex deletedPaths)
      else writeIndex indexPath (MkIndex deletedPaths)

-- | Permanently deletes the paths from the trash.
--
-- @since 0.1
permDel :: Maybe FilePath -> HashSet FilePath -> IO ()
permDel mtrash paths = do
  trashHome <- trashOrDefault mtrash
  let indexPath = getIndexPath trashHome
  index@(MkIndex indexMap) <- readIndex indexPath

  -- NOTE: No buffering on input so we can read a single char w/o requiring a
  -- newline to end the input (which then gets passed to getChar, which
  -- interferes with subsequent calls).
  IO.hSetBuffering IO.stdin IO.NoBuffering

  -- NOTE: No buffering on output so the "Permanently delete..." string gets
  -- printed w/o the newline.
  IO.hSetBuffering IO.stdout IO.NoBuffering

  toDelete <- searchIndexForPermDel trashHome paths index

  deletedPathsRef <- newIORef Map.empty

  -- move trash paths back to original location
  let deletePathsFn = for_ toDelete $ \pd -> do
        let pdStr = (renderString . layoutCompact . (Pretty.line <>) . pretty) pd
        putStrLn pdStr
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
    writeIndex indexPath (MkIndex $ Map.difference indexMap deletedPaths)

-- | Reads the index at either the specified or default location. If the
-- file does not exist, returns empty.
--
-- @since 0.1
getIndex :: Maybe FilePath -> IO Index
getIndex mtrash = do
  trashHome <- trashOrDefault mtrash
  let indexPath = getIndexPath trashHome
  Dir.doesFileExist indexPath >>= \case
    True -> readIndex indexPath
    False -> pure mempty

-- | Retrieves statistics for the trash directory.
--
-- @since 0.1
getStatistics :: Maybe FilePath -> IO Statistics
getStatistics = trashOrDefault >=> getStats

-- | @restore trash p@ restores the trashed path @\<trash\>\/p@ to its original
-- location. If @trash@ is not given then we look in the default location
-- e.g. @~\/.trash@.
--
-- @since 0.1
restore :: Maybe FilePath -> HashSet FilePath -> IO ()
restore mtrash paths = do
  trashHome <- trashOrDefault mtrash
  let indexPath = getIndexPath trashHome
  index@(MkIndex indexMap) <- readIndex indexPath
  toRestore <- searchIndexForRestore trashHome paths index

  restoredPathsRef <- newIORef Map.empty

  -- move trash paths back to original location
  let restorePathsFn = for_ toRestore $ \pd -> do
        pathTypeToRenameFn
          (pd ^. #pathType)
          (pd ^. #trashPath)
          (pd ^. #originalPath)
        modifyIORef' restoredPathsRef (Map.insert (pd ^. #trashPath) pd)

  -- override old index
  restorePathsFn `finally` do
    -- TODO: mask all exceptions and enforce no-throw
    restoredPaths <- readIORef restoredPathsRef
    writeIndex indexPath (MkIndex $ Map.difference indexMap restoredPaths)

-- | Empties the trash. Deletes the index file.
--
-- @since 0.1
empty :: Maybe FilePath -> IO ()
empty = trashOrDefault >=> Dir.removeDirectoryRecursive
