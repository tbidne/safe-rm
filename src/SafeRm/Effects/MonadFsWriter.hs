-- | Provides the 'MonadFsWriter' typeclass.
--
-- @since 0.1
module SafeRm.Effects.MonadFsWriter
  ( MonadFsWriter (..),
  )
where

import Control.Monad.Trans (MonadTrans (lift))
import Data.ByteString qualified as BS
import SafeRm.Exceptions (withStackTracing)
import SafeRm.Prelude
import System.IO qualified as IO
import UnliftIO.Directory qualified as Dir

-- | Represents a readable filesystem.
--
-- @since 0.1
class Monad m => MonadFsWriter m where
  -- | Writes to a file.
  --
  -- @since 0.1
  writeFile :: HasCallStack => FilePath -> ByteString -> m ()

  -- | Appends to a file.
  --
  -- @since 0.1
  appendFile :: HasCallStack => FilePath -> ByteString -> m ()

  -- | Opens a file.
  --
  -- @since 0.1
  openFile :: FilePath -> IOMode -> m Handle

  -- | Writes to the handle.
  --
  -- @since 0.1
  hPut :: Handle -> ByteString -> m ()

  -- | Closes a handle.
  --
  -- @since 0.1
  hClose :: Handle -> m ()

  -- | Flushes a handle.
  --
  -- @since 0.1
  hFlush :: Handle -> m ()

  -- | Renames a file.
  --
  -- @since 0.1
  renameFile :: HasCallStack => FilePath -> FilePath -> m ()

  -- | Renames a directory.
  --
  -- @since 0.1
  renameDirectory :: HasCallStack => FilePath -> FilePath -> m ()

  -- | Removes a path.
  --
  -- @since 0.1
  removePathForcibly :: HasCallStack => FilePath -> m ()

  -- | Removes a directory.
  --
  -- @since 0.1
  removeDirectoryRecursive :: HasCallStack => FilePath -> m ()

  -- | Creates a directory.
  --
  -- @since 0.1
  createDirectoryIfMissing :: HasCallStack => Bool -> FilePath -> m ()

-- | @since 0.1
instance MonadFsWriter IO where
  writeFile f = withStackTracing . BS.writeFile f
  appendFile f = withStackTracing . BS.appendFile f
  openFile f = withStackTracing . IO.openFile f
  hPut h = withStackTracing . BS.hPut h
  hClose = withStackTracing . IO.hClose
  hFlush = withStackTracing . IO.hFlush
  renameFile f = withStackTracing . Dir.renameFile f
  renameDirectory f = withStackTracing . Dir.renameDirectory f
  removePathForcibly = withStackTracing . Dir.removePathForcibly
  removeDirectoryRecursive = withStackTracing . Dir.removeDirectoryRecursive
  createDirectoryIfMissing b = withStackTracing . Dir.createDirectoryIfMissing b

-- | @since 0.1
instance MonadFsWriter m => MonadFsWriter (ReaderT env m) where
  writeFile f = lift . writeFile f
  appendFile f = lift . appendFile f
  openFile f = lift . openFile f
  hPut f = lift . hPut f
  hClose = lift . hClose
  hFlush = lift . hFlush
  renameFile f = lift . renameFile f
  renameDirectory f = lift . renameDirectory f
  removePathForcibly = lift . removePathForcibly
  removeDirectoryRecursive = lift . removeDirectoryRecursive
  createDirectoryIfMissing b = lift . createDirectoryIfMissing b
