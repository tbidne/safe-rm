{-# LANGUAGE OverloadedLists #-}

-- | Provides functionality for moving a file to a trash location.
--
-- @since 0.1
module Del
  ( del,
    restore,
  )
where

import Control.DeepSeq (NFData)
import Control.Exception (Exception (displayException), throwIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Csv (FromField, FromRecord, HasHeader (NoHeader), ToField, ToRecord)
import Data.Csv qualified as Csv
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Text.Encoding.Error qualified as TEncError
import Data.Vector (Vector)
import Data.Vector qualified as V
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

-- | Attempts to move the given filepath to @~\/.Trash@ and write an entry in
-- @~\/.Trash\/.index@.
--
-- __Throws:__
--
-- * 'PathNotFoundError': The given path was not found.
-- * 'RenameDuplicateError': No unique file names are found (i.e. the limit of 65,535
--   is reached).
--
-- @since 0.1
del :: FilePath -> IO ()
del fp = do
  trashHome <- getTrashHome
  Dir.createDirectoryIfMissing False trashHome
  pd <- toPathData trashHome fp
  mvToTrash pd
  appendIndex trashHome pd

-- | Attempts to restore the given filepath.
--
-- __Throws:__
--
-- * 'ReadIndexError': Error reading the trash index file.
-- * 'PathNotInIndexError': Filepath not found in trash index.
-- * 'DuplicateIndexPathsError': Filepath found multiple times in trash index.
-- * 'TrashPathNotFoundError': Filepath not found in trash path.
-- * 'PathExistsError': Filepath's original path already exists.
--
-- @since 0.1
restore :: FilePath -> IO ()
restore fp = do
  trashHome <- getTrashHome
  let indexPath = getIndexPath trashHome
  index <- readIndex indexPath
  (MkPathData {trashPath, originalPath, pathType}, newIndex) <- searchIndex trashHome fp index

  -- verify no collision at original path
  pathTypeToExistFn pathType originalPath >>= \case
    True -> throwIO $ MkPathExistsError originalPath
    False -> pure ()

  -- move trash back to original location
  pathTypeToRenameFn pathType trashPath originalPath

  -- override old index
  writeIndex indexPath (V.toList newIndex)

-- | Attempts to read the trash index file.
--
-- __Throws:__
--
-- * 'ReadIndexError': If there is an error reading the trash index.
--
-- @since 0.1
readIndex :: FilePath -> IO (Vector PathData)
readIndex indexPath =
  BS.readFile indexPath
    >>= ( \case
            Left err -> throwIO $ MkReadIndexError err
            Right xs -> pure xs
        )
      . Csv.decode NoHeader
      . BSL.fromStrict

-- | NOTE: The key corresponds to the (possibly renamed) /trash/ path, not
-- necessarily the original name.
--
-- __Throws:__
--
-- * 'PathNotInIndexError'
-- * 'DuplicateIndexPathsError'
-- * 'TrashPathNotFoundError'
--
-- @since 0.1
searchIndex ::
  -- | The trash location e.g. @~\/.Trash@.
  FilePath ->
  -- | The top-level trash key to restore e.g. @~\/.Trash\/foo@.
  FilePath ->
  -- | The trash index.
  Vector PathData ->
  -- | The trash data matching the input key and the index with the key
  -- removed.
  IO (PathData, Vector PathData)
searchIndex trashHome key index = do
  -- search index for key
  (pd@MkPathData {pathType}, nonMatches) <- do
    case V.partition isMatch index of
      ([x], rest) -> pure (x, rest)
      ([], _) -> throwIO $ MkPathNotInIndexError key
      (matches, _) -> throwIO $ MkDuplicateIndexPathsError matches

  -- verify contents actually exist in trash
  pathTypeToExistFn pathType trashKey >>= \case
    False -> throwIO $ MkTrashPathNotFoundError trashKey
    True -> pure (pd, nonMatches)
  where
    trashKey = trashHome </> key
    isMatch MkPathData {trashPath} = trashPath == trashKey

-- | For a given filepath, attempts to capture the following data:
--
-- * Canonical path.
-- * Unique name to be used in the trash directory.
-- * File/directory type.
--
-- __Throws:__
--
-- * 'PathNotFoundError'
-- * 'RenameDuplicateError'
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

-- | Appends the path data to the trash index. This is intended to be used
-- after a successful move to the trash.
--
-- @since 0.1
appendIndex :: FilePath -> PathData -> IO ()
appendIndex trashHome = BS.appendFile index . BSL.toStrict . Csv.encode . (: [])
  where
    index = getIndexPath trashHome

-- | Appends the path data to the trash index. This is intended to be used
-- after a successful move to the trash.
--
-- @since 0.1
writeIndex :: FilePath -> [PathData] -> IO ()
writeIndex indexPath = BS.writeFile indexPath . BSL.toStrict . Csv.encode

-- | Renames a path based on its type.
--
-- @since 0.1
pathTypeToRenameFn :: PathType -> FilePath -> FilePath -> IO ()
pathTypeToRenameFn PathTypeFile = Dir.renameFile
pathTypeToRenameFn PathTypeDirectory = Dir.renameDirectory

-- | Tests a path's existence.
--
-- @since 0.1
pathTypeToExistFn :: PathType -> FilePath -> IO Bool
pathTypeToExistFn PathTypeFile = Dir.doesFileExist
pathTypeToExistFn PathTypeDirectory = Dir.doesDirectoryExist

-- | Retrieves the trash directory.
--
-- @since 0.1
getTrashHome :: IO FilePath
getTrashHome = (</> ".Trash") <$> Dir.getHomeDirectory

-- | Retrieves the trash index path based on the given directory path.
--
-- @since 0.1
getIndexPath :: FilePath -> FilePath
getIndexPath = (</> ".index.csv")

-- | Converts UTF8 'ByteString' to 'String'. Decoding is lenient.
--
-- @since 0.1
bsToStr :: ByteString -> String
bsToStr = T.unpack . TEnc.decodeUtf8With TEncError.lenientDecode

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

-- | Error when attempting to read the index.
--
-- @since 0.1
newtype ReadIndexError = MkReadIndexError String
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
instance Exception ReadIndexError where
  displayException (MkReadIndexError err) = "Error reading index: " <> err

-- | Path not found in index.
--
-- @since 0.1
newtype PathNotInIndexError = MkPathNotInIndexError FilePath
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
instance Exception PathNotInIndexError where
  displayException (MkPathNotInIndexError fp) =
    "Path not found in trash index: " <> fp

-- | Duplicate trash paths found.
--
-- @since 0.1
newtype DuplicateIndexPathsError = MkDuplicateIndexPathsError (Vector PathData)
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
instance Exception DuplicateIndexPathsError where
  displayException (MkDuplicateIndexPathsError matches) =
    mconcat
      [ "Wanted a unique match in the index, found ",
        show $ V.length matches,
        ": ",
        showVec matches
      ]
    where
      showVec :: Vector PathData -> String
      showVec = V.foldl' f ""
      f acc pd = "\n - " <> showPd pd <> acc
      showPd MkPathData {trashPath, originalPath} =
        mconcat
          [ "trash path: ",
            trashPath,
            "\n   original path: ",
            originalPath,
            "\n"
          ]

-- | Path not found in trash.
--
-- @since 0.1
newtype TrashPathNotFoundError = MkTrashPathNotFoundError FilePath
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
instance Exception TrashPathNotFoundError where
  displayException (MkTrashPathNotFoundError fp) = "Path not found in trash: " <> fp

-- | Path already exists.
--
-- @since 0.1
newtype PathExistsError = MkPathExistsError FilePath
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
instance Exception PathExistsError where
  displayException (MkPathExistsError fp) =
    "File already exists at the original path: " <> fp
