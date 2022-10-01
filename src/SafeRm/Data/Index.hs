-- | Provides types.
--
-- @since 0.1
module SafeRm.Data.Index
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
import Data.ByteString.Char8 qualified as Char8
import Data.Csv (HasHeader (HasHeader))
import Data.Csv qualified as Csv
import Data.Csv.Streaming (Records (Cons, Nil))
import Data.Csv.Streaming qualified as Csv.Streaming
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.List qualified as L
import SafeRm.Data.PathData (PathData, sortDefault)
import SafeRm.Data.PathData qualified as PathData
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex (TrashHome, TrashIndex, TrashName),
    _MkPathI,
  )
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Exceptions
  ( ExceptionI (MkExceptionI),
    ExceptionIndex
      ( DuplicateIndexPath,
        PathNotFound,
        ReadIndex,
        RestoreCollision,
        TrashPathNotFound
      ),
  )
import SafeRm.Prelude
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
readIndex ::
  MonadIO m =>
  PathI TrashIndex ->
  m Index
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
  forall m.
  MonadIO m =>
  -- | If true, errors if there is a collision between a found trash path
  -- and its original path.
  Bool ->
  -- | Trash home. Used to verify that found trash paths actually exist.
  PathI TrashHome ->
  -- | The top-level trash keys to find e.g. @foo@ for @~\/.trash\/foo@.
  HashSet (PathI TrashName) ->
  -- | The trash index.
  Index ->
  -- | The trash data matching the input keys.
  m ([SomeException], HashSet PathData)
searchIndex errIfOrigCollision trashHome keys (MkIndex index) =
  Set.foldl' foldKeys (pure mempty) trashKeys
  where
    -- NOTE: drop trailing slashes to match our index's schema
    trashKeys = Set.map (Paths.liftPathI' FP.dropTrailingPathSeparator) keys
    foldKeys ::
      m ([SomeException], HashSet PathData) ->
      PathI TrashName ->
      m ([SomeException], HashSet PathData)
    foldKeys macc trashKey = do
      acc@(exs, found) <- macc
      case Map.lookup trashKey index of
        Nothing ->
          pure $
            prependEx
              (MkExceptionI @PathNotFound (view _MkPathI trashKey))
              acc
        Just pd -> do
          nonExtant <- trashNonExtant trashHome pd
          if nonExtant
            then
              pure $
                prependEx
                  (MkExceptionI @TrashPathNotFound (trashHome, pd ^. #fileName))
                  acc
            else do
              -- optional collision detection
              collision <- mCollisionErr pd
              if collision
                then
                  pure $
                    prependEx
                      ( MkExceptionI @RestoreCollision
                          (pd ^. #fileName, pd ^. #originalPath)
                      )
                      acc
                else pure (exs, Set.insert pd found)
    mCollisionErr =
      if errIfOrigCollision
        then PathData.originalPathExists
        else const (pure False)
    prependEx ex = over' _1 (toException ex :)

-- | Reads a csv index file and applies the fold function to each
-- 'PathData' encountered. The fold function allows 'IO' in case it is needed
-- to check any conditions (e.g. trash path actually exists).
--
-- @since 0.1
readIndexWithFold ::
  forall m a.
  (MonadIO m, Monoid a) =>
  -- | Fold function.
  (m a -> PathData -> m a) ->
  -- | Path to index file.
  PathI TrashIndex ->
  m a
readIndexWithFold foldFn indexPath@(MkPathI fp) =
  ((liftIO . BS.readFile) >=> runFold (pure mempty) . decode) fp
  where
    decode = Csv.Streaming.decode HasHeader . BSL.fromStrict
    -- NOTE: We fold over the Records manually because its Foldable instance
    -- swallows errors, whereas we would like to report any encountered
    -- immediately.
    runFold :: m a -> Records PathData -> m a
    -- Base case, we have parsed everything with no errors nor unconsumed
    -- input
    runFold macc (Nil Nothing "") = macc
    -- End of stream w/ an error.
    runFold _ (Nil (Just err) rest) =
      throwIO $
        MkExceptionI @ReadIndex
          ( indexPath,
            mconcat
              [ err,
                " at \"",
                lbsToStr rest,
                "\""
              ]
          )
    -- No errors but there is unconsumed input. This is probably impossible,
    -- but just to cover all cases...
    runFold _ (Nil _ rest) =
      throwIO $
        MkExceptionI @ReadIndex
          (indexPath, "Unconsumed input: " <> lbsToStr rest)
    -- Encountered an error.
    runFold _ (Cons (Left err) _) =
      throwIO $ MkExceptionI @ReadIndex (indexPath, err)
    -- Inductive case, run fold and recurse
    runFold macc (Cons (Right x) rest) = runFold (foldFn macc x) rest

    lbsToStr = Char8.unpack . BSL.toStrict

-- | Verifies that the 'PathData'\'s @fileName@ does not exist in the
-- hashmap.
--
-- @since 0.1
throwIfDuplicates ::
  MonadIO m =>
  PathI TrashIndex ->
  HashMap (PathI TrashName) PathData ->
  PathData ->
  m ()
throwIfDuplicates indexPath trashMap pd =
  when (fileName `Map.member` trashMap) $
    throwIO $
      MkExceptionI @DuplicateIndexPath (indexPath, fileName)
  where
    fileName = pd ^. #fileName

-- | Verifies that the 'PathData'\'s @fileName@ actually exists.
--
-- @since 0.1
throwIfTrashNonExtant :: MonadIO m => PathI TrashHome -> PathData -> m ()
throwIfTrashNonExtant trashHome pd = do
  exists <- PathData.trashPathExists trashHome pd
  unless exists $
    throwIO $
      MkExceptionI @TrashPathNotFound (trashHome, filePath)
  where
    filePath = pd ^. #fileName

trashNonExtant :: MonadIO m => PathI TrashHome -> PathData -> m Bool
trashNonExtant fp = fmap not . PathData.trashPathExists fp

-- | Appends the path data to the trash index. The header is not included.
--
-- @since 0.1
appendIndex :: MonadIO m => PathI TrashIndex -> Index -> m ()
appendIndex (MkPathI indexPath) =
  (liftIO . BS.appendFile indexPath)
    . BSL.toStrict
    . Csv.encode
    . Map.elems
    . view #unIndex

-- | Writes the path data to the trash index, overwriting the index if it
-- exists. The header is included.
--
-- @since 0.1
writeIndex :: MonadIO m => PathI TrashIndex -> Index -> m ()
writeIndex (MkPathI indexPath) =
  (liftIO . BS.writeFile indexPath)
    . BSL.toStrict
    . Csv.encodeDefaultOrderedByName
    . Map.elems
    . view #unIndex
