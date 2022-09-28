{-# LANGUAGE OverloadedLists #-}

-- | Provides types.
--
-- @since 0.1
module Del.Data.PathData
  ( PathData (..),

    -- * Creation
    toPathData,
    mvOriginalToTrash,

    -- * Existence
    trashPathExists,
    originalPathExists,

    -- * Deletion
    mvTrashToOriginal,
    deletePathData,

    -- * Sorting
    sortDefault,
    sortCreated,
    sortName,

    -- * Miscellaneous
    headerNames,
    pathDataToTrashPath,
  )
where

import Data.Csv
  ( DefaultOrdered (headerOrder),
    FromNamedRecord,
    FromRecord,
    ToNamedRecord,
    ToRecord,
    (.:),
    (.=),
  )
import Data.Csv qualified as Csv
import Data.HashMap.Strict qualified as Map
import Del.Data.PathType (PathType (PathTypeDirectory, PathTypeFile))
import Del.Data.Paths
  ( PathI (MkPathI),
    PathIndex (OriginalName, TrashHome, TrashName, TrashPath),
    (<//>),
    _MkPathI,
  )
import Del.Data.Paths qualified as Paths
import Del.Data.Timestamp (Timestamp)
import Del.Exceptions
  ( ExceptionI (MkExceptionI),
    ExceptionIndex (PathNotFound, RenameDuplicate),
  )
import Del.Prelude
import GHC.Exts (IsList (Item))
import System.Directory qualified as Dir
import System.FilePath qualified as FP

-- | Data for a path.
--
-- @since 0.1
data PathData = MkPathData
  { -- | The type of the path.
    --
    -- @since 0.1
    pathType :: !PathType,
    -- | The path to be used in the trash directory.
    --
    -- @since 0.1
    fileName :: !(PathI TrashName),
    -- | The original path on the file system.
    --
    -- @since 0.1
    originalPath :: !(PathI OriginalName),
    -- | Time this entry was created.
    --
    -- @since 0.1
    created :: !Timestamp
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
      Hashable,
      -- | @since 0.1
      NFData,
      -- | @since 0.1
      ToRecord
    )

-- | @since 0.1
instance FromNamedRecord PathData where
  parseNamedRecord m =
    MkPathData
      <$> m .: "type"
      <*> m .: "fileName"
      <*> m .: "original"
      <*> m .: "created"

-- | @since 0.1
instance ToNamedRecord PathData where
  toNamedRecord pd = Map.fromList $ zipWith (flip ($)) headerNames labelFn
    where
      labelFn =
        [ \x -> x .= (pd ^. #pathType),
          \x -> x .= (pd ^. #fileName),
          \x -> x .= (pd ^. #originalPath),
          \x -> x .= (pd ^. #created)
        ]

-- | @since 0.1
instance DefaultOrdered PathData where
  headerOrder _ = headerNames

-- | @since 0.1
instance Pretty PathData where
  pretty pd = vsep strs <+> line
    where
      strs = zipWith (flip ($)) headerNames labelFn
      labelFn =
        [ \x -> x <> ":     " <+> pretty (pd ^. #pathType),
          \x -> x <> ":     " <+> pretty (pd ^. #fileName % _MkPathI),
          \x -> x <> ": " <+> pretty (pd ^. #originalPath % _MkPathI),
          \x -> x <> ":  " <+> pretty (pd ^. #created)
        ]

-- | Header names.
--
-- @since 0.1
headerNames :: (IsList a, IsString (Item a)) => a
headerNames = ["Type", "Name", "Original", "Created"]

-- | For a given filepath, attempts to capture the following data:
--
-- * Canonical path.
-- * Unique name to be used in the trash directory.
-- * File/directory type.
--
-- @since 0.1
toPathData :: Timestamp -> PathI TrashHome -> PathI OriginalName -> IO PathData
toPathData currTime trashHome originalPath = do
  origPath <- Paths.liftPathIM Dir.canonicalizePath originalPath
  -- NOTE: need to get the file name here because fp could refer to an
  -- absolute path. In this case, </> returns the 2nd arg which is absolutely
  -- not what we want.
  --
  -- This works for directories too because canonicalizePath drops the
  -- trailing slashes.
  let fileName = Paths.liftPathI FP.takeFileName origPath
  uniqPath <- mkUniqPath (trashHome <//> fileName)
  let uniqName = Paths.liftPathI FP.takeFileName uniqPath
  isFile <- Paths.applyPathI Dir.doesFileExist origPath
  if isFile
    then
      pure
        MkPathData
          { fileName = uniqName,
            originalPath = origPath,
            pathType = PathTypeFile,
            created = currTime
          }
    else do
      isDir <- Paths.applyPathI Dir.doesDirectoryExist origPath
      if isDir
        then
          pure
            -- NOTE: ensure paths do not have trailing slashes so that we can
            -- ensure later lookups succeed (requires string equality)
            MkPathData
              { fileName =
                  Paths.liftPathI FP.dropTrailingPathSeparator uniqName,
                originalPath =
                  Paths.liftPathI FP.dropTrailingPathSeparator origPath,
                pathType = PathTypeDirectory,
                created = currTime
              }
        else throwIO $ MkExceptionI @PathNotFound (origPath ^. _MkPathI)

-- | Ensures the filepath @p@ is unique. If @p@ collides with another path,
-- we iteratively try appending numbers, stopping once we find a unique path.
-- For example, for duplicate "file", we will try
--
-- @
-- "file1", "file2", "file3", ...
-- @
--
-- @since 0.1
mkUniqPath :: PathI TrashName -> IO (PathI TrashName)
mkUniqPath fp = do
  b <- Paths.applyPathI Dir.doesPathExist fp
  if b
    then go 1
    else pure fp
  where
    go :: Word16 -> IO (PathI TrashName)
    go !counter
      | counter == maxBound = throwIO $ MkExceptionI @RenameDuplicate fp
      | otherwise = do
          let fp' = fp <> MkPathI (show counter)
          b <- Paths.applyPathI Dir.doesPathExist fp'
          if b
            then go (counter + 1)
            else pure fp'

-- | Sorts by the created date then the name.
--
-- @since 0.1
sortDefault :: PathData -> PathData -> Ordering
sortDefault x y = case sortCreated x y of
  EQ -> sortName x y
  other -> other

-- | Sorts by the created date.
--
-- @since 0.1
sortCreated :: PathData -> PathData -> Ordering
sortCreated = mapOrd (view #created)

-- | Sorts by the name.
--
-- @since 0.1
sortName :: PathData -> PathData -> Ordering
sortName = mapOrd (view #fileName)

mapOrd :: Ord b => (a -> b) -> a -> a -> Ordering
mapOrd f x y = f x `compare` f y

-- | Moves the 'PathData' to the trash.
--
-- @since 0.1
mvTrashToOriginal :: PathI TrashHome -> PathData -> IO ()
mvTrashToOriginal (MkPathI trashHome) pd =
  renameFn trashPath (pd ^. #originalPath % _MkPathI)
  where
    trashPath = trashHome </> (pd ^. #fileName % _MkPathI)
    renameFn = case pd ^. #pathType of
      PathTypeFile -> Dir.renameFile
      PathTypeDirectory -> Dir.renameDirectory

-- | Permanently deletes the 'PathData'.
--
-- @since 0.1
deletePathData :: PathI TrashHome -> PathData -> IO ()
deletePathData (MkPathI trashHome) pd = Dir.removePathForcibly trashPath
  where
    trashPath = trashHome </> (pd ^. #fileName % _MkPathI)

-- | Renames a path based on its type.
--
-- @since 0.1
mvOriginalToTrash :: PathI TrashHome -> PathData -> IO ()
mvOriginalToTrash trashHome pd =
  renameFn (pd ^. #originalPath % _MkPathI) trashPath
  where
    MkPathI trashPath = pathDataToTrashPath trashHome pd
    renameFn = case pd ^. #pathType of
      PathTypeFile -> Dir.renameFile
      PathTypeDirectory -> Dir.renameDirectory

-- | Returns 'True' if the 'PathData''s 'fileName' corresponds to a real path
-- that exists in 'TrashHome'.
--
-- @since 0.1
trashPathExists :: PathI TrashHome -> PathData -> IO Bool
trashPathExists (MkPathI trashHome) pd = existsFn trashPath
  where
    trashPath = trashHome </> (pd ^. #fileName % _MkPathI)
    existsFn = case pd ^. #pathType of
      PathTypeFile -> Dir.doesFileExist
      PathTypeDirectory -> Dir.doesDirectoryExist

-- | Returns 'True' if the 'PathData''s 'originalPath' corresponds to a real
-- path that exists.
--
-- @since 0.1
originalPathExists :: PathData -> IO Bool
originalPathExists pd = existsFn (pd ^. #originalPath % _MkPathI)
  where
    existsFn = case pd ^. #pathType of
      PathTypeFile -> Dir.doesFileExist
      PathTypeDirectory -> Dir.doesDirectoryExist

-- | Gives the 'PathData's full trash path in the given 'TrashHome'.
--
-- @since 0.1
pathDataToTrashPath :: PathI TrashHome -> PathData -> PathI TrashPath
pathDataToTrashPath trashHome = (trashHome <//>) . view #fileName
