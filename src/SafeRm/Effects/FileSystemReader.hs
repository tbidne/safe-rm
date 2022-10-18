-- | Provides the 'FileSystemReader' typeclass.
--
-- @since 0.1
module SafeRm.Effects.FileSystemReader
  ( FileSystemReader (..),
  )
where

import Control.Monad.Trans (MonadTrans (lift))
import Data.ByteString qualified as BS
import Data.Bytes (Bytes (MkBytes), Size (B))
import SafeRm.Exceptions
  ( ExceptionI (MkExceptionI),
    ExceptionIndex (PathNotFound),
    wrapCS,
  )
import SafeRm.Prelude
import UnliftIO.Directory qualified as Dir

-- | Represents a readable filesystem.
--
-- @since 0.1
class Monad m => FileSystemReader m where
  -- | Retrieves a file's size.
  --
  -- @since 0.1
  getFileSize :: HasCallStack => FilePath -> m (Bytes B Natural)

  -- | Reads a file.
  --
  -- @since 0.1
  readFile :: HasCallStack => FilePath -> m ByteString

  -- | Tests a file's existence.
  --
  -- @since 0.1
  doesFileExist :: HasCallStack => FilePath -> m Bool

  -- | Tests a directory's existence.
  --
  -- @since 0.1
  doesDirectoryExist :: HasCallStack => FilePath -> m Bool

  -- | Tests a path's existence.
  --
  -- @since 0.1
  doesPathExist :: HasCallStack => FilePath -> m Bool

  -- | Canonicalize a path.
  --
  -- @since 0.1
  canonicalizePath :: HasCallStack => FilePath -> m FilePath

  -- | Lists a directory.
  --
  -- @since 0.1
  listDirectory :: HasCallStack => FilePath -> m [FilePath]

-- | @since 0.1
instance FileSystemReader IO where
  getFileSize f = MkBytes . natToInt <$> Dir.getFileSize f
    where
      natToInt x
        | x < 0 =
            error $
              mconcat
                [ "[SafeRm.Effects.FileSystemReader]",
                  "getFileSize returned ",
                  show x,
                  " bytes for file: ",
                  f
                ]
        | otherwise = fromIntegral x

  readFile :: FilePath -> IO ByteString
  readFile f = do
    exists <- doesFileExist f
    unless exists $
      throwCS @_ @(ExceptionI PathNotFound) $
        MkExceptionI f
    wrapCS (BS.readFile f)

  doesFileExist = wrapCS . Dir.doesFileExist

  doesDirectoryExist = wrapCS . Dir.doesDirectoryExist

  doesPathExist = wrapCS . Dir.doesPathExist

  canonicalizePath = wrapCS . Dir.canonicalizePath
  listDirectory = wrapCS . Dir.listDirectory

-- | @since 0.1
instance FileSystemReader m => FileSystemReader (ReaderT env m) where
  getFileSize = lift . getFileSize
  readFile = lift . readFile
  doesFileExist = lift . doesFileExist
  doesDirectoryExist = lift . doesDirectoryExist
  doesPathExist = lift . doesPathExist
  canonicalizePath = lift . canonicalizePath
  listDirectory = lift . listDirectory
