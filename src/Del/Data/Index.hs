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
import Del.Data.PathData qualified as PathData
import Del.Data.Paths (PathI (..), PathIndex (..), _MkPathI)
import Del.Data.Paths qualified as Paths
import Del.Exceptions
  ( DuplicateIndexPathsError (..),
    PathNotFoundError (..),
    ReadIndexError (..),
    RestoreCollisionError (..),
    TrashPathNotFoundError (..),
  )
import Del.Prelude
import System.FilePath qualified as FP

-- | Index that stores the trash data.
--
-- @since 0.1
newtype Index = MkIndex
  { unIndex :: HashMap (PathI TrashName) PathData
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
    via (HashMap (PathI TrashName) PathData)

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
readIndex :: PathI TrashIndex -> IO Index
readIndex indexPath =
  fmap MkIndex . readIndexWithFold foldVec $ indexPath
  where
    trashHome = Paths.indexToHome indexPath
    foldVec macc pd = do
      acc <- macc
      throwIfDuplicates indexPath acc pd
      throwIfTrashNonExtant trashHome pd
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
  -- | Trash home. Used to verify that found trash paths actually exist.
  PathI TrashHome ->
  -- | The top-level trash keys to find e.g. @foo@ for @~\/.trash\/foo@.
  --
  -- REVIEW: Should this be a non-empty set?
  HashSet (PathI TrashName) ->
  -- | The trash index.
  Index ->
  -- | The trash data matching the input keys.
  IO (HashSet PathData)
searchIndex errIfOrigCollision trashHome keys (MkIndex index) =
  Set.foldl' foldKeys (pure mempty) trashKeys
  where
    -- NOTE: drop trailing slashes to match our index's schema
    trashKeys = Set.map (Paths.liftPathI FP.dropTrailingPathSeparator) keys
    foldKeys :: IO (HashSet PathData) -> PathI TrashName -> IO (HashSet PathData)
    foldKeys macc trashKey = do
      acc <- macc
      case Map.lookup trashKey index of
        Nothing -> throwIO $ MkPathNotFoundError (view _MkPathI trashKey)
        Just pd -> do
          throwIfTrashNonExtant trashHome pd

          -- optional collision detection
          mCollisionErr pd
          pure $ Set.insert pd acc
    mCollisionErr =
      if errIfOrigCollision
        then throwIfOrigCollision
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
  PathI TrashIndex ->
  IO a
readIndexWithFold foldFn indexPath@(MkPathI fp) =
  (BS.readFile >=> runFold . decode) fp
  where
    decode = Csv.decode HasHeader . BSL.fromStrict
    runFold = \case
      Left err -> throwIO $ MkReadIndexError (indexPath, err)
      Right vec -> V.foldl' foldFn (pure mempty) vec

-- | Verifies that the 'PathData''s 'originalPath' does not collide with
-- an existing path.
--
-- @since 0.1
throwIfOrigCollision :: PathData -> IO ()
throwIfOrigCollision pd = do
  exists <- PathData.originalPathExists pd
  if exists
    then throwIO $ MkRestoreCollisionError originalPath
    else pure ()
  where
    originalPath = pd ^. #originalPath

-- | Verifies that the 'PathData''s 'fileName' does not exist in the
-- hashmap.
--
-- @since 0.1
throwIfDuplicates ::
  PathI TrashIndex ->
  HashMap (PathI TrashName) PathData ->
  PathData ->
  IO ()
throwIfDuplicates indexPath trashMap pd =
  if fileName `Map.member` trashMap
    then throwIO $ MkDuplicateIndexPathsError (indexPath, fileName)
    else pure ()
  where
    fileName = pd ^. #fileName

-- | Verifies that the 'PathData'\'s 'fileName' actually exists.
--
-- @since 0.1
throwIfTrashNonExtant :: PathI TrashHome -> PathData -> IO ()
throwIfTrashNonExtant trashHome pd = do
  exists <- PathData.trashPathExists trashHome pd
  if not exists
    then throwIO $ MkTrashPathNotFoundError (trashHome, filePath)
    else pure ()
  where
    filePath = pd ^. #fileName

-- | Appends the path data to the trash index. This is intended to be used
-- after a successful move to the trash.
--
-- @since 0.1
appendIndex :: PathI TrashIndex -> Index -> IO ()
appendIndex (MkPathI indexPath) =
  BS.appendFile indexPath
    . BSL.toStrict
    . Csv.encode
    . Map.elems
    . view #unIndex

-- | Writes the path data to the trash index. This is intended to be used
-- after a successful move to the trash.
--
-- @since 0.1
writeIndex :: PathI TrashIndex -> Index -> IO ()
writeIndex (MkPathI indexPath) =
  BS.writeFile indexPath
    . BSL.toStrict
    . Csv.encodeDefaultOrderedByName
    . Map.elems
    . view #unIndex
