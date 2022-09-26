-- | Provides types.
--
-- @since 0.1
module Del.Data.Index
  ( Index (..),

    -- * Reading
    readIndex,
    searchIndex,

    -- * Writing
    appendIndex,
    writeIndex,
  )
where

import Data.ByteString qualified as BS
import Data.ByteString qualified as BSL
import Data.Csv (HasHeader (HasHeader))
import Data.Csv qualified as Csv
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.List qualified as L
import Data.Vector qualified as V
import Del.Data.PathData (PathData (..), sortDefault)
import Del.Data.PathType (pathTypeToExistFn)
import Del.Exceptions
  ( DuplicateIndexPathsError (..),
    PathExistsError (..),
    PathNotFoundError (..),
    ReadIndexError (..),
    TrashPathNotFoundError (..),
  )
import Del.Prelude
import System.FilePath qualified as FP

-- | Index that stores the trash data.
--
-- @since 0.1
newtype Index = MkIndex
  { unIndex :: HashMap FilePath PathData
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
      NFData
    )
  deriving
    ( -- | @since 0.1
      Semigroup,
      -- | @since 0.1
      Monoid
    )
    via (HashMap FilePath PathData)

-- | @since 0.1
instance Pretty Index where
  pretty =
    vsep
      . fmap pretty
      . L.sortBy sortDefault
      . Map.elems
      . view #unIndex

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
