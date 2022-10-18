-- | Provides the 'FileSystemWriter' typeclass.
--
-- @since 0.1
module SafeRm.Effects.FileSystemWriter
  ( FileSystemWriter (..),
  )
where

import Control.Monad.Trans (MonadTrans (lift))
import Data.ByteString qualified as BS
import SafeRm.Exceptions (wrapCS)
import SafeRm.Prelude
import UnliftIO.Directory qualified as Dir

-- | Represents a readable filesystem.
--
-- @since 0.1
class Monad m => FileSystemWriter m where
  -- | Writes to a file.
  --
  -- @since 0.1
  writeFile :: HasCallStack => FilePath -> ByteString -> m ()

  -- | Appends to a file.
  --
  -- @since 0.1
  appendFile :: HasCallStack => FilePath -> ByteString -> m ()

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
instance FileSystemWriter IO where
  writeFile f = wrapCS . BS.writeFile f
  appendFile f = wrapCS . BS.appendFile f
  renameFile f = wrapCS . Dir.renameFile f
  renameDirectory f = wrapCS . Dir.renameDirectory f
  removePathForcibly = wrapCS . Dir.removePathForcibly
  removeDirectoryRecursive = wrapCS . Dir.removeDirectoryRecursive
  createDirectoryIfMissing b = wrapCS . Dir.createDirectoryIfMissing b

-- | @since 0.1
instance FileSystemWriter m => FileSystemWriter (ReaderT env m) where
  writeFile f = lift . writeFile f
  appendFile f = lift . appendFile f
  renameFile f = lift . renameFile f
  renameDirectory f = lift . renameDirectory f
  removePathForcibly = lift . removePathForcibly
  removeDirectoryRecursive = lift . removeDirectoryRecursive
  createDirectoryIfMissing b = lift . createDirectoryIfMissing b
