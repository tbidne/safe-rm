-- | Provides functionality for moving a file to a trash location.
--
-- @since 0.1
module Del
  ( del,
  )
where

import Control.DeepSeq (NFData)
import Control.Exception (Exception (displayException), throwIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Csv (FromField, FromRecord, ToField, ToRecord)
import Data.Csv qualified as Csv
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Text.Encoding.Error qualified as TEncError
import Data.Word (Word16)
import GHC.Generics (Generic)
import System.Directory qualified as Dir
import System.FilePath ((</>))

-- | Path type.
--
-- @since 0.1
data PathType
  = -- | File type.
    --
    -- @since 0.1
    PathTypeFile
  | -- | Directory type
    --
    -- @since 0.1
    PathTypeDirectory
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance FromField PathType where
  parseField s
    | s == "file" = pure PathTypeFile
    | s == "directory" = pure PathTypeDirectory
    | otherwise = fail $ "Expected 'file' or 'directory'. Received: " <> bsToStr s

-- | @since 0.1
instance ToField PathType where
  toField PathTypeFile = "file"
  toField PathTypeDirectory = "directory"

-- | Data for a path.
--
-- @since 0.1
data PathData = MkPathData
  { -- | The path to be used in the trash directory.
    --
    -- @since 0.1
    trashPath :: !FilePath,
    -- | The original path on the file system.
    --
    -- @since 0.1
    originalPath :: !FilePath,
    -- | The type of the path.
    --
    -- @since 0.1
    pathType :: !PathType
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      FromRecord,
      -- | @since 0.1
      NFData,
      -- | @since 0.1
      ToRecord
    )

-- | Error when searching for a path.
--
-- @since 0.1
newtype PathNotFoundError = MkPathNotFoundError FilePath
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance Exception PathNotFoundError where
  displayException (MkPathNotFoundError fp) = "Path not found: " <> fp

-- | Error when attempting to rename a duplicate path.
--
-- @since 0.1
newtype RenameDuplicateError = MkRenameDuplicateError FilePath
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance Exception RenameDuplicateError where
  displayException (MkRenameDuplicateError fp) = "Failed renaming duplicate file: " <> fp

-- | Attempts to move the given filepath to @~\/.Trash@, and write an entry in
-- @~\/.Trash\/.index@.
--
-- @since 0.1
del :: FilePath -> IO ()
del fp = do
  trashHome <- getTrashHome
  Dir.createDirectoryIfMissing False trashHome
  pd <- toPathData trashHome fp
  mvToTrash pd
  writeIndex trashHome pd

-- | For a given filepath, attempts to capture the following data:
--
-- * Canonical path.
-- * Unique name to be used in the trash directory.
-- * File/directory type.
--
-- __Throws:__
--
-- * 'PathNotFoundError': if the given path is not found
-- * 'RenameDuplicateError': if no unique file names are found (i.e. the limit of 65,535
--   is reached).
--
-- @since 0.1
toPathData :: FilePath -> FilePath -> IO PathData
toPathData trashHome fp = do
  cp <- Dir.canonicalizePath fp

  pathType' <- do
    isFile <- Dir.doesFileExist cp
    if isFile
      then pure PathTypeFile
      else do
        isDir <- Dir.doesDirectoryExist cp
        if isDir
          then pure PathTypeDirectory
          else throwIO $ MkPathNotFoundError cp

  uniqPath <- uniqName (trashHome </> fp)
  pure $
    MkPathData
      { trashPath = uniqPath,
        originalPath = cp,
        pathType = pathType'
      }

-- | Ensures the filepath @p@ is unique. If @p@ collides with another path,
-- we iteratively try appending numbers, stopping once we find a unique path.
-- For example, for duplicate "file", we will try
--
-- @
-- "file1", "file2", "file3", ...
-- @
--
-- __Throws:__
--
-- * 'RenameDuplicateError': if no unique file names are found (i.e. the limit of 65,535
--   is reached).
--
-- @since 0.1
uniqName :: FilePath -> IO FilePath
uniqName fp = do
  b <- Dir.doesPathExist fp
  if b
    then go 1
    else pure fp
  where
    go :: Word16 -> IO FilePath
    go !counter
      | counter == maxBound = throwIO $ MkRenameDuplicateError fp
      | otherwise = do
          let fp' = fp <> show counter
          b <- Dir.doesPathExist fp'
          if b
            then go (counter + 1)
            else pure fp'

-- | Moves the file to the trash.
--
-- @since 0.1
mvToTrash :: PathData -> IO ()
mvToTrash MkPathData {trashPath, originalPath, pathType} = renameFn originalPath trashPath
  where
    renameFn = pathTypeToRenameFn pathType
    pathTypeToRenameFn PathTypeFile = Dir.renameFile
    pathTypeToRenameFn PathTypeDirectory = Dir.renameDirectory

-- | Writes the path data to the trash index. This is intended to be used
-- after a successful move to the trash.
--
-- @since 0.1
writeIndex :: FilePath -> PathData -> IO ()
writeIndex trashHome pd = BS.appendFile index (BSL.toStrict $ Csv.encode [pd])
  where
    index = getIndex trashHome

-- | Retrieves the trash directory.
--
-- @since 0.1
getTrashHome :: IO FilePath
getTrashHome = (</> ".Trash") <$> Dir.getHomeDirectory

-- | Retrieves the trash index path based on the given directory path.
--
-- @since 0.1
getIndex :: FilePath -> FilePath
getIndex = (</> ".index")

-- | Converts UTF8 'ByteString' to 'String'. Decoding is lenient.
--
-- @since 0.1
bsToStr :: ByteString -> String
bsToStr = T.unpack . TEnc.decodeUtf8With TEncError.lenientDecode
