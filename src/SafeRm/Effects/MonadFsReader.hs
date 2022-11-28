-- | Provides the 'MonadFsReader' typeclass.
--
-- @since 0.1
module SafeRm.Effects.MonadFsReader
  ( MonadFsReader (..),
  )
where

import Control.Monad.Trans (MonadTrans (lift))
import Data.ByteString qualified as BS
import Data.Sequence (Seq (Empty, (:<|)))
import PathSize
  ( Config
      ( MkConfig,
        exclude,
        filesOnly,
        maxDepth,
        numPaths,
        searchAll,
        strategy
      ),
    SubPathData (MkSubPathData),
    findLargestPaths,
  )
import SafeRm.Effects.MonadCallStack (throwCallStack)
import SafeRm.Exception
  ( PathNotFoundE (MkPathNotFoundE),
    withStackTracing,
  )
import SafeRm.Prelude
import UnliftIO.Directory qualified as Dir

-- | Represents a readable filesystem.
--
-- @since 0.1
class Monad m => MonadFsReader m where
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
instance MonadFsReader IO where
  getFileSize f = withStackTracing $ MkBytes <$> getPathSize f

  readFile :: FilePath -> IO ByteString
  readFile f = do
    exists <- doesFileExist f
    unless exists $
      throwCallStack $
        MkPathNotFoundE f
    withStackTracing (BS.readFile f)

  doesFileExist = withStackTracing . Dir.doesFileExist

  doesDirectoryExist = withStackTracing . Dir.doesDirectoryExist

  doesPathExist = withStackTracing . Dir.doesPathExist

  canonicalizePath = withStackTracing . Dir.canonicalizePath

  listDirectory = withStackTracing . Dir.listDirectory

-- NOTE: The below instance is only used in file-utils. SafeRmT has its
-- own instance.

-- | @since 0.1
instance MonadFsReader m => MonadFsReader (ReaderT env m) where
  getFileSize = lift . getFileSize
  readFile = lift . readFile
  doesFileExist = lift . doesFileExist
  doesDirectoryExist = lift . doesDirectoryExist
  doesPathExist = lift . doesPathExist
  canonicalizePath = lift . canonicalizePath
  listDirectory = lift . listDirectory

getPathSize :: FilePath -> IO Natural
getPathSize path =
  findLargestPaths cfg path >>= \case
    (Empty, MkSubPathData (x :<| _)) -> pure $ x ^. #size
    (errs, _) ->
      throwString $
        "Error retrieving size: "
          <> foldl' (\s e -> s <> "\n" <> displayException e) "" errs
  where
    cfg =
      MkConfig
        { searchAll = True,
          maxDepth = Just 0,
          exclude = mempty,
          filesOnly = False,
          numPaths = Just 1,
          strategy = mempty
        }
