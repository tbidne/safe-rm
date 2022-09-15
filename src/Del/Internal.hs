-- | Provides internal utility functions
--
-- @since 0.1
module Del.Internal
  ( -- * Index functions
    readIndex,
    searchIndexForRestore,
    searchIndexForPermDel,
    appendIndex,
    writeIndex,
    getIndexPath,

    -- * Path type functions
    pathTypeToExistFn,
    pathTypeToRenameFn,

    -- * Trash functions
    trashOrDefault,
    getTrashHome,
    mvToTrash,

    -- * Utilities
    toPathData,
  )
where

import Control.Exception (throwIO)
import Control.Monad ((>=>))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Csv (HasHeader (NoHeader))
import Data.Csv qualified as Csv
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as Map
import Data.HashSet (HashSet)
import Data.HashSet qualified as Set
import Data.Vector qualified as V
import Data.Word (Word16)
import Del.Exceptions
  ( DuplicateIndexPathsError (MkDuplicateIndexPathsError),
    PathExistsError (MkPathExistsError),
    PathNotFoundError (MkPathNotFoundError),
    ReadIndexError (MkReadIndexError),
    RenameDuplicateError (MkRenameDuplicateError),
    TrashPathNotFoundError (MkTrashPathsNotFoundError),
  )
import Del.Types
  ( Index (MkIndex, unIndex),
    PathData (MkPathData, originalPath, pathType, trashPath),
    PathType (PathTypeDirectory, PathTypeFile),
  )
import Optics.Core ((^.))
import System.Directory qualified as Dir
import System.FilePath ((</>))

-- | Attempts to read the trash index file. If successful, guarantees:
--
-- * All trash paths are unique.
-- * Every index entry corresponds to a path in the trash directory.
--
-- @since 0.1
readIndex :: FilePath -> IO Index
readIndex = fmap MkIndex . readIndexWithFold foldVec
  where
    foldVec macc pd = do
      acc <- macc
      noDuplicates acc pd
      trashPathExists pd
      pure $ Map.insert trashPath pd acc
      where
        trashPath = pd ^. #trashPath

-- | 'searchIndex' with collision decision.
--
-- @since 0.1
searchIndexForRestore ::
  FilePath -> HashSet FilePath -> Index -> IO (HashSet PathData)
searchIndexForRestore = searchIndex True

-- | 'searchIndex' with no collision decision.
--
-- @since 0.1
searchIndexForPermDel ::
  FilePath -> HashSet FilePath -> Index -> IO (HashSet PathData)
searchIndexForPermDel = searchIndex False

-- | Searches the trash index for keys.
--
-- @since 0.1
searchIndex ::
  -- | If true, errors if there is a collision between a found trash path
  -- and its original path.
  Bool ->
  -- | The trash location e.g. @~\/.trash@.
  FilePath ->
  -- | The top-level trash keys to find e.g. @foo@ for @~\/.trash\/foo@.
  --
  -- REVIEW: Should this be a non-empty set?
  HashSet FilePath ->
  -- | The trash index.
  Index ->
  -- | The trash data matching the input keys.
  IO (HashSet PathData)
searchIndex errIfOrigCollision trashHome keys (MkIndex index) =
  Set.foldl' foldKeys (pure mempty) trashKeys
  where
    trashKeys = Set.map (trashHome </>) keys
    foldKeys :: IO (HashSet PathData) -> FilePath -> IO (HashSet PathData)
    foldKeys macc trashKey = do
      acc <- macc
      case Map.lookup trashKey index of
        Nothing -> throwIO $ MkTrashPathsNotFoundError trashKey
        Just pd -> do
          -- optional collision detection
          mCollisionErr pd
          pure $ Set.insert pd acc
    mCollisionErr =
      if errIfOrigCollision
        then noOrigPathCollision
        else (const (pure ()))

-- | Reads a csv index file and applies the fold function to each
-- 'PathData' encountered. The fold function allows 'IO' in case it is needed
-- to check any conditions (e.g. trash path actually exists).
--
-- @since 0.1
readIndexWithFold ::
  Monoid a =>
  -- | Fold function.
  (IO a -> PathData -> IO a) ->
  -- | Path to index file.
  FilePath ->
  IO a
readIndexWithFold foldFn = BS.readFile >=> runFold . decode
  where
    decode = Csv.decode NoHeader . BSL.fromStrict
    runFold = \case
      Left err -> throwIO $ MkReadIndexError err
      Right vec -> V.foldl' foldFn (pure mempty) vec

-- | Verifies that the 'PathData''s 'originalPath' does not collide with
-- an existing path.
--
-- @since 0.1
noOrigPathCollision :: PathData -> IO ()
noOrigPathCollision pd = do
  exists <- pathTypeToExistFn (pd ^. #pathType) originalPath
  if exists
    then throwIO $ MkPathExistsError originalPath
    else pure ()
  where
    originalPath = pd ^. #originalPath

-- | Verifies that the 'PathData''s 'trashPath' does not exist in the
-- hashmap.
--
-- @since 0.1
noDuplicates ::
  HashMap FilePath PathData ->
  PathData ->
  IO ()
noDuplicates trashMap pd =
  if trashPath `Map.member` trashMap
    then throwIO $ MkDuplicateIndexPathsError trashPath
    else pure ()
  where
    trashPath = pd ^. #trashPath

-- | Verifies that the 'PathData'\'s 'trashPath' actually exists.
--
-- @since 0.1
trashPathExists :: PathData -> IO ()
trashPathExists pd = do
  exists <- pathTypeToExistFn (pd ^. #pathType) trashPath
  if not exists
    then throwIO $ MkTrashPathsNotFoundError trashPath
    else pure ()
  where
    trashPath = pd ^. #trashPath

-- | Tests a path's existence.
--
-- @since 0.1
pathTypeToExistFn :: PathType -> FilePath -> IO Bool
pathTypeToExistFn PathTypeFile = Dir.doesFileExist
pathTypeToExistFn PathTypeDirectory = Dir.doesDirectoryExist

-- | Renames a path based on its type.
--
-- @since 0.1
pathTypeToRenameFn :: PathType -> FilePath -> FilePath -> IO ()
pathTypeToRenameFn PathTypeFile = Dir.renameFile
pathTypeToRenameFn PathTypeDirectory = Dir.renameDirectory

-- | For a given filepath, attempts to capture the following data:
--
-- * Canonical path.
-- * Unique name to be used in the trash directory.
-- * File/directory type.
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
mvToTrash pd = renameFn (pd ^. #originalPath) (pd ^. #trashPath)
  where
    renameFn = pathTypeToRenameFn (pd ^. #pathType)

-- | Appends the path data to the trash index. This is intended to be used
-- after a successful move to the trash.
--
-- @since 0.1
appendIndex :: FilePath -> PathData -> IO ()
appendIndex trashHome =
  BS.appendFile index
    . BSL.toStrict
    -- TODO: Include a header. Somehow we have to know if the file exists
    -- (or create a blank one first)
    . Csv.encode
    . (: [])
  where
    index = getIndexPath trashHome

-- | Appends the path data to the trash index. This is intended to be used
-- after a successful move to the trash.
--
-- @since 0.1
writeIndex :: FilePath -> Index -> IO ()
writeIndex indexPath =
  BS.writeFile indexPath
    . BSL.toStrict
    -- TODO: write a header
    . Csv.encode
    . Map.elems
    . unIndex

-- | If the argument is given, returns it. Otherwise searches for the default
-- trash location.
--
-- @since 0.1
trashOrDefault :: Maybe FilePath -> IO FilePath
trashOrDefault = maybe getTrashHome pure

-- | Retrieves the trash directory.
--
-- @since 0.1
getTrashHome :: IO FilePath
getTrashHome = (</> ".trash") <$> Dir.getHomeDirectory

-- | Retrieves the trash index path based on the given directory path.
--
-- @since 0.1
getIndexPath :: FilePath -> FilePath
getIndexPath = (</> ".index.csv")
