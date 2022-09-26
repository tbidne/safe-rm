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
    getStats,

    -- * Utilities
    allM1,
  )
where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Bytes (Bytes (MkBytes), Size (B))
import Data.Bytes qualified as Bytes
import Data.Csv (HasHeader (HasHeader))
import Data.Csv qualified as Csv
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.Vector qualified as V
import Del.Data.Index (Index (..))
import Del.Data.PathData (PathData (..))
import Del.Data.PathType (PathType (..))
import Del.Data.Statistics (Statistics (..))
import Del.Exceptions
  ( DuplicateIndexPathsError (MkDuplicateIndexPathsError),
    PathExistsError (MkPathExistsError),
    PathNotFoundError (MkPathNotFoundError),
    ReadIndexError (MkReadIndexError),
    TrashPathNotFoundError (MkTrashPathsNotFoundError),
  )
import Del.Prelude
import System.Directory qualified as Dir
import System.FilePath qualified as FP

-- | Attempts to read the trash index file. If successful, guarantees:
--
-- * All trash paths are unique.
-- * Every index entry corresponds to a path in the trash directory.
--
-- @since 0.1
readIndex :: FilePath -> IO Index
readIndex fp = do
  fmap MkIndex . readIndexWithFold foldVec $ fp
  where
    trashHome = FP.takeDirectory fp
    foldVec macc pd = do
      acc <- macc
      noDuplicates acc pd
      trashPathExists trashHome pd
      pure $ Map.insert fileName pd acc
      where
        fileName = pd ^. #fileName

-- | 'searchIndex' with collision decision.
--
-- @since 0.1
searchIndexForRestore :: HashSet FilePath -> Index -> IO (HashSet PathData)
searchIndexForRestore = searchIndex True

-- | 'searchIndex' with no collision decision.
--
-- @since 0.1
searchIndexForPermDel :: HashSet FilePath -> Index -> IO (HashSet PathData)
searchIndexForPermDel = searchIndex False

-- | Searches the trash index for keys.
--
-- @since 0.1
searchIndex ::
  -- | If true, errors if there is a collision between a found trash path
  -- and its original path.
  Bool ->
  -- | The top-level trash keys to find e.g. @foo@ for @~\/.trash\/foo@.
  --
  -- REVIEW: Should this be a non-empty set?
  HashSet FilePath ->
  -- | The trash index.
  Index ->
  -- | The trash data matching the input keys.
  IO (HashSet PathData)
searchIndex errIfOrigCollision keys (MkIndex index) =
  Set.foldl' foldKeys (pure mempty) trashKeys
  where
    -- NOTE: drop trailing slashes to match our index's schema
    trashKeys = Set.map FP.dropTrailingPathSeparator keys
    foldKeys :: IO (HashSet PathData) -> FilePath -> IO (HashSet PathData)
    foldKeys macc trashKey = do
      acc <- macc
      case Map.lookup trashKey index of
        Nothing -> throwIO $ MkPathNotFoundError trashKey
        Just pd -> do
          -- optional collision detection
          mCollisionErr pd
          pure $ Set.insert pd acc
    mCollisionErr =
      if errIfOrigCollision
        then noOrigPathCollision
        else const (pure ())

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
    decode = Csv.decode HasHeader . BSL.fromStrict
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

-- | Verifies that the 'PathData''s 'fileName' does not exist in the
-- hashmap.
--
-- @since 0.1
noDuplicates ::
  HashMap FilePath PathData ->
  PathData ->
  IO ()
noDuplicates trashMap pd =
  if fileName `Map.member` trashMap
    then throwIO $ MkDuplicateIndexPathsError fileName
    else pure ()
  where
    fileName = pd ^. #fileName

-- | Verifies that the 'PathData'\'s 'fileName' actually exists.
--
-- @since 0.1
trashPathExists :: FilePath -> PathData -> IO ()
trashPathExists trashHome pd = do
  exists <- pathTypeToExistFn (pd ^. #pathType) filePath
  if not exists
    then throwIO $ MkTrashPathsNotFoundError filePath
    else pure ()
  where
    filePath = trashHome </> pd ^. #fileName

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

-- | Moves the file to the trash.
--
-- @since 0.1
mvToTrash :: FilePath -> PathData -> IO ()
mvToTrash trashHome pd = renameFn (pd ^. #originalPath) trashPath
  where
    trashPath = trashHome </> pd ^. #fileName
    renameFn = pathTypeToRenameFn (pd ^. #pathType)

-- | Appends the path data to the trash index. This is intended to be used
-- after a successful move to the trash.
--
-- @since 0.1
appendIndex :: FilePath -> Index -> IO ()
appendIndex indexPath =
  BS.appendFile indexPath
    . BSL.toStrict
    . Csv.encode
    . Map.elems
    . view #unIndex

-- | Writes the path data to the trash index. This is intended to be used
-- after a successful move to the trash.
--
-- @since 0.1
writeIndex :: FilePath -> Index -> IO ()
writeIndex indexPath =
  BS.writeFile indexPath
    . BSL.toStrict
    . Csv.encodeDefaultOrderedByName
    . Map.elems
    . view #unIndex

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

-- | Returns stats on the trash directory.
--
-- @since 0.1
getStats :: FilePath -> IO Statistics
getStats fp = do
  -- TODO: This _should_ be the same as the index length (corresponds exactly
  -- to top-level paths except .index.csv). We do this instead of parsing
  -- the entire index for performance.
  --
  -- We may want to actually verify this invariant here, failing if there is
  -- a mismatch.
  numEntries <- (\xs -> length xs - 1) <$> Dir.listDirectory fp
  allFiles <- getAllFiles fp
  allSizes <- toDouble <$> foldl' sumFileSizes (pure 0) allFiles
  let numFiles = length allFiles - 1
      normalized = Bytes.normalize (MkBytes @B allSizes)
  pure $
    MkStatistics
      { numEntries = toNat numEntries,
        numFiles = toNat numFiles,
        size = normalized
      }
  where
    sumFileSizes macc f = do
      !acc <- macc
      sz <- Dir.getFileSize f
      pure $ acc + sz
    toDouble :: Integer -> Double
    toDouble = fromIntegral
    toNat :: Int -> Natural
    toNat = fromIntegral

getAllFiles :: FilePath -> IO [FilePath]
getAllFiles fp =
  Dir.doesFileExist fp >>= \case
    True -> pure [fp]
    False ->
      Dir.doesDirectoryExist fp >>= \case
        True ->
          Dir.listDirectory fp
            >>= fmap join
              . traverse (getAllFiles . (fp </>))
        False -> throwIO $ MkPathNotFoundError fp

-- | 'allM' that must have at least one 'True'.
--
-- @since 0.1
allM1 :: Monad m => NonEmpty (m Bool) -> m Bool
allM1 (m :| ms) =
  m >>= \case
    True -> allM ms
    False -> pure False

-- | 'all' lifted to monads.
--
-- @since 0.1
allM :: (Foldable t, Monad m) => t (m Bool) -> m Bool
allM = foldr f (pure True)
  where
    f m acc =
      m >>= \case
        True -> acc
        False -> pure False
