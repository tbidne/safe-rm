{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides types.
--
-- @since 0.1
module SafeRm.Data.PathData
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

import Data.Bytes (_MkBytes)
import Data.Csv
  ( DefaultOrdered (headerOrder),
    FromNamedRecord,
    FromRecord,
    ToNamedRecord,
    ToRecord,
    (.!),
    (.:),
    (.=),
  )
import Data.Csv qualified as Csv
import Data.HashMap.Strict qualified as Map
import GHC.Exts (IsList)
import GHC.Exts qualified as Exts
import SafeRm.Data.PathType (PathType (PathTypeDirectory, PathTypeFile))
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex (OriginalPath, TrashHome, TrashName, TrashPath),
    (<//>),
  )
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Effects.MonadCallStack (MonadCallStack, throwCallStack)
import SafeRm.Effects.MonadFsReader
  ( MonadFsReader
      ( canonicalizePath,
        doesDirectoryExist,
        doesFileExist,
        doesPathExist,
        getFileSize
      ),
  )
import SafeRm.Effects.MonadFsWriter
  ( MonadFsWriter
      ( removePathForcibly,
        renameDirectory,
        renameFile
      ),
  )
import SafeRm.Effects.MonadSystemTime (Timestamp)
import SafeRm.Exception
  ( PathNotFoundE (MkPathNotFoundE),
    RenameDuplicateE (MkRenameDuplicateE),
    RestoreCollisionE (MkRestoreCollisionE),
  )
import SafeRm.Prelude
import SafeRm.Utils qualified as U
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
    originalPath :: !(PathI OriginalPath),
    -- | The size of the file or directory.
    --
    -- @since 0.1
    size :: !(Bytes B Natural),
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
      Hashable,
      -- | @since 0.1
      NFData
    )

makeFieldLabelsNoPrefix ''PathData

-- | @since 0.1
instance FromRecord PathData where
  parseRecord m =
    MkPathData
      <$> m .! 0
      <*> m .! 1
      <*> m .! 2
      <*> (MkBytes <$> m .! 3)
      <*> m .! 4

-- | @since 0.1
instance FromNamedRecord PathData where
  parseNamedRecord m =
    MkPathData
      <$> m .: "type"
      <*> m .: "fileName"
      <*> m .: "original"
      <*> (MkBytes <$> m .: "size")
      <*> m .: "created"

-- | @since 0.1
instance ToRecord PathData where
  toRecord pd =
    [ Csv.toField (pd ^. #pathType),
      Csv.toField (pd ^. #fileName),
      Csv.toField (pd ^. #originalPath),
      Csv.toField (pd ^. #size % _MkBytes),
      Csv.toField (pd ^. #created)
    ]

-- | @since 0.1
instance ToNamedRecord PathData where
  toNamedRecord pd = Map.fromList $ zipWith (flip ($)) headerNames labelFn
    where
      labelFn =
        [ \x -> x .= (pd ^. #pathType),
          \x -> x .= (pd ^. #fileName),
          \x -> x .= (pd ^. #originalPath),
          \x -> x .= (pd ^. #size % _MkBytes),
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
          \x -> x <> ":     " <+> pretty (pd ^. #fileName % #unPathI),
          \x -> x <> ": " <+> pretty (pd ^. #originalPath % #unPathI),
          \x -> x <> ": " <+> pretty (U.normalizedFormat $ pd ^. #size),
          \x -> x <> ":  " <+> pretty (pd ^. #created)
        ]

-- | Header names.
--
-- @since 0.1
headerNames :: (IsList a, IsString (Exts.Item a)) => a
headerNames = ["Type", "Name", "Original", "Size", "Created"]

-- | For a given filepath, attempts to capture the following data:
--
-- * Canonical path.
-- * Unique name to be used in the trash directory.
-- * File/directory type.
--
-- @since 0.1
toPathData ::
  ( MonadFsReader m,
    HasCallStack,
    MonadCallStack m,
    MonadIO m
  ) =>
  Timestamp ->
  PathI TrashHome ->
  PathI OriginalPath ->
  m PathData
toPathData currTime trashHome origPath = do
  originalPath <- Paths.liftPathIF' canonicalizePath origPath
  -- NOTE: need to get the file name here because fp could refer to an
  -- absolute path. In this case, </> returns the 2nd arg which is absolutely
  -- not what we want.
  --
  -- This works for directories too because canonicalizePath drops the
  -- trailing slashes.
  let fileName = Paths.liftPathI' FP.takeFileName originalPath
  uniqPath <- mkUniqPath (trashHome <//> fileName)
  let uniqName = Paths.liftPathI' FP.takeFileName uniqPath
  isFile <- Paths.applyPathI doesFileExist originalPath
  -- TODO: it would be nice if this listed the recursive size of the
  -- directory
  size <- getFileSize (originalPath ^. #unPathI)
  if isFile
    then
      pure
        MkPathData
          { fileName = uniqName,
            originalPath,
            pathType = PathTypeFile,
            size,
            created = currTime
          }
    else do
      isDir <- Paths.applyPathI doesDirectoryExist origPath
      if isDir
        then
          pure
            -- NOTE: ensure paths do not have trailing slashes so that we can
            -- ensure later lookups succeed (requires string equality)
            MkPathData
              { fileName =
                  Paths.liftPathI' FP.dropTrailingPathSeparator uniqName,
                originalPath =
                  Paths.liftPathI' FP.dropTrailingPathSeparator origPath,
                pathType = PathTypeDirectory,
                size,
                created = currTime
              }
        else throwCallStack $ MkPathNotFoundE (origPath ^. #unPathI)

-- | Ensures the filepath @p@ is unique. If @p@ collides with another path,
-- we iteratively try appending numbers, stopping once we find a unique path.
-- For example, for duplicate "file", we will try
--
-- @
-- "file1", "file2", "file3", ...
-- @
--
-- @since 0.1
mkUniqPath ::
  forall m.
  ( MonadFsReader m,
    HasCallStack,
    MonadCallStack m,
    MonadIO m
  ) =>
  PathI TrashName ->
  m (PathI TrashName)
mkUniqPath fp = do
  b <- Paths.applyPathI doesPathExist fp
  if b
    then go 1
    else pure fp
  where
    go :: HasCallStack => Word16 -> m (PathI TrashName)
    go !counter
      | counter == maxBound =
          throwCallStack $ MkRenameDuplicateE fp
      | otherwise = do
          let fp' = fp <> MkPathI (mkSuffix counter)
          b <- Paths.applyPathI doesPathExist fp'
          if b
            then go (counter + 1)
            else pure fp'
    mkSuffix i = " (" <> show i <> ")"

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

-- | Moves the 'PathData'\'s @fileName@ back to its @originalPath@.
--
-- @since 0.1
mvTrashToOriginal ::
  ( MonadFsReader m,
    MonadFsWriter m,
    HasCallStack,
    MonadCallStack m,
    MonadIO m
  ) =>
  PathI TrashHome ->
  PathData ->
  m ()
mvTrashToOriginal (MkPathI trashHome) pd = do
  exists <- originalPathExists pd
  when exists $
    throwCallStack $
      MkRestoreCollisionE fileName originalPath
  renameFn trashPath (pd ^. #originalPath % #unPathI)
  where
    originalPath = pd ^. #originalPath
    fileName = pd ^. #fileName
    trashPath = trashHome </> (fileName ^. #unPathI)
    renameFn = case pd ^. #pathType of
      PathTypeFile -> renameFile
      PathTypeDirectory -> renameDirectory

-- | Permanently deletes the 'PathData'.
--
-- @since 0.1
deletePathData ::
  MonadFsWriter m =>
  PathI TrashHome ->
  PathData ->
  m ()
deletePathData (MkPathI trashHome) pd = removePathForcibly trashPath
  where
    trashPath = trashHome </> (pd ^. #fileName % #unPathI)

-- | Moves the 'PathData'\'s @originalPath@ to the trash.
--
-- @since 0.1
mvOriginalToTrash ::
  (MonadFsWriter m, HasCallStack) =>
  PathI TrashHome ->
  PathData ->
  m ()
mvOriginalToTrash trashHome pd =
  renameFn (pd ^. #originalPath % #unPathI) trashPath
  where
    MkPathI trashPath = pathDataToTrashPath trashHome pd
    renameFn = case pd ^. #pathType of
      PathTypeFile -> renameFile
      PathTypeDirectory -> renameDirectory

-- | Returns 'True' if the 'PathData'\'s @fileName@ corresponds to a real path
-- that exists in 'TrashHome'.
--
-- @since 0.1
trashPathExists ::
  (MonadFsReader m, HasCallStack) =>
  PathI TrashHome ->
  PathData ->
  m Bool
trashPathExists (MkPathI trashHome) pd = existsFn trashPath
  where
    trashPath = trashHome </> (pd ^. #fileName % #unPathI)
    existsFn = case pd ^. #pathType of
      PathTypeFile -> doesFileExist
      PathTypeDirectory -> doesDirectoryExist

-- | Returns 'True' if the 'PathData'\'s @originalPath@ corresponds to a real
-- path that exists.
--
-- @since 0.1
originalPathExists ::
  (MonadFsReader m, HasCallStack) =>
  PathData ->
  m Bool
originalPathExists pd = existsFn (pd ^. #originalPath % #unPathI)
  where
    existsFn = case pd ^. #pathType of
      PathTypeFile -> doesFileExist
      PathTypeDirectory -> doesDirectoryExist

-- | Gives the 'PathData'\'s full trash path in the given 'TrashHome'.
--
-- @since 0.1
pathDataToTrashPath :: PathI TrashHome -> PathData -> PathI TrashPath
pathDataToTrashPath trashHome = (trashHome <//>) . view #fileName
