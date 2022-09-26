{-# LANGUAGE OverloadedLists #-}

-- | Provides types.
--
-- @since 0.1
module Del.Data.PathData
  ( PathData (..),

    -- * Creation
    toPathData,

    -- * Sorting
    sortDefault,
    sortCreated,
    sortName,
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
import Del.Data.PathType (PathType (..))
import Del.Data.Timestamp (Timestamp (..))
import Del.Exceptions (PathNotFoundError (..), RenameDuplicateError (MkRenameDuplicateError))
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
    fileName :: !FilePath,
    -- | The original path on the file system.
    --
    -- @since 0.1
    originalPath :: !FilePath,
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
          \x -> x <> ":     " <+> pretty (pd ^. #fileName),
          \x -> x <> ": " <+> pretty (pd ^. #originalPath),
          \x -> x <> ":  " <+> pretty (pd ^. #created)
        ]

headerNames :: (IsList a, IsString (Item a)) => a
headerNames = ["type", "name", "original", "created"]

-- | For a given filepath, attempts to capture the following data:
--
-- * Canonical path.
-- * Unique name to be used in the trash directory.
-- * File/directory type.
--
-- @since 0.1
toPathData :: Timestamp -> FilePath -> FilePath -> IO PathData
toPathData currTime trashHome fp = do
  origPath <- Dir.canonicalizePath fp
  -- NOTE: need to get the file name here because fp could refer to an
  -- absolute path. In this case, </> returns the 2nd arg which is absolutely
  -- not what we want.
  --
  -- This works for directories too because canonicalizePath drops the
  -- trailing slashes.
  let fileName = FP.takeFileName origPath
  uniqPath <- mkUniqPath (trashHome </> fileName)
  let uniqName = FP.takeFileName uniqPath
  isFile <- Dir.doesFileExist origPath
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
      isDir <- Dir.doesDirectoryExist origPath
      if isDir
        then
          pure
            -- NOTE: ensure paths do not have trailing slashes so that we can
            -- ensure later lookups succeed (requires string equality)
            MkPathData
              { fileName = FP.dropTrailingPathSeparator uniqName,
                originalPath = FP.dropTrailingPathSeparator origPath,
                pathType = PathTypeDirectory,
                created = currTime
              }
        else throwIO $ MkPathNotFoundError origPath

-- | Ensures the filepath @p@ is unique. If @p@ collides with another path,
-- we iteratively try appending numbers, stopping once we find a unique path.
-- For example, for duplicate "file", we will try
--
-- @
-- "file1", "file2", "file3", ...
-- @
--
-- @since 0.1
mkUniqPath :: FilePath -> IO FilePath
mkUniqPath fp = do
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
