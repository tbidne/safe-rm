{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

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

import Data.ByteString qualified as BSL
import Data.ByteString.Char8 qualified as Char8
import Data.Csv (HasHeader (HasHeader))
import Data.Csv qualified as Csv
import Data.Csv.Streaming (Records (Cons, Nil))
import Data.Csv.Streaming qualified as Csv.Streaming
import Data.HashMap.Strict qualified as HMap
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Text.Encoding.Error qualified as TEncError
import SafeRm.Data.PathData (PathData, sortDefault)
import SafeRm.Data.PathData qualified as PathData
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex (TrashHome, TrashIndex, TrashName),
  )
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Data.UniqueSeq (UniqueSeq)
import SafeRm.Data.UniqueSeq qualified as USeq
import SafeRm.Effects.FileSystemReader (FileSystemReader (readFile))
import SafeRm.Effects.FileSystemWriter
  ( FileSystemWriter
      ( appendFile,
        writeFile
      ),
  )
import SafeRm.Effects.Logger (LoggerContext, addNamespace)
import SafeRm.Exceptions
  ( ExceptionI (MkExceptionI),
    ExceptionIndex
      ( DuplicateIndexPath,
        PathNotFound,
        ReadIndex,
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

makeFieldLabelsNoPrefix ''Index

-- | @since 0.1
instance Pretty Index where
  pretty =
    vsep
      . fmap pretty
      . L.sortBy sortDefault
      . HMap.elems
      . view #unIndex

-- | Attempts to read the trash index file. If successful, guarantees:
--
-- * All trash paths are unique.
-- * Every index entry corresponds to a path in the trash directory.
--
-- @since 0.1
readIndex ::
  ( FileSystemReader m,
    LoggerContext m,
    MonadIO m
  ) =>
  PathI TrashIndex ->
  m Index
readIndex indexPath = addNamespace "readIndex" $ do
  $(logDebug) ("Index path: " <> T.pack (indexPath ^. #unPathI))
  fmap MkIndex . readIndexWithFold foldVec $ indexPath
  where
    trashHome = Paths.indexToHome indexPath
    foldVec macc pd = do
      $(logDebug) ("Found: " <> showt pd)
      acc <- macc
      throwIfDuplicates indexPath acc pd
      throwIfTrashNonExtant trashHome pd
      pure $ HMap.insert fileName pd acc
      where
        fileName = pd ^. #fileName

-- | Searches the trash index for keys.
--
-- @since 0.1
searchIndex ::
  HasCallStack =>
  -- | The top-level trash keys to find e.g. @foo@ for @~\/.trash\/foo@.
  UniqueSeq (PathI TrashName) ->
  -- | The trash index.
  Index ->
  -- | The trash data matching the input keys.
  ([SomeException], UniqueSeq PathData)
searchIndex keys (MkIndex index) =
  foldr foldKeys mempty trashKeys
  where
    -- NOTE: drop trailing slashes to match our index's schema
    trashKeys = USeq.map (Paths.liftPathI' FP.dropTrailingPathSeparator) keys
    foldKeys ::
      HasCallStack =>
      PathI TrashName ->
      ([SomeException], UniqueSeq PathData) ->
      ([SomeException], UniqueSeq PathData)
    foldKeys trashKey acc@(exs, found) =
      case HMap.lookup trashKey index of
        Nothing ->
          -- NOTE: since this is the only exception type we do not actually
          -- have to do this; we could simply return the offending trashKey.
          -- Nevertheless we turn these into exceptions since it makes the
          -- callers' lives easier.
          prependEx
            (MkExceptionI @PathNotFound (view #unPathI trashKey) ?callStack)
            acc
        Just pd -> (exs, USeq.append found pd)
    prependEx ex = over' _1 (toException ex :)

-- | Reads a csv index file and applies the fold function to each
-- 'PathData' encountered. The fold function allows 'IO' in case it is needed
-- to check any conditions (e.g. trash path actually exists).
--
-- @since 0.1
readIndexWithFold ::
  forall m a.
  ( FileSystemReader m,
    HasCallStack,
    LoggerContext m,
    MonadIO m,
    Monoid a
  ) =>
  -- | Fold function.
  (m a -> PathData -> m a) ->
  -- | Path to index file.
  PathI TrashIndex ->
  m a
readIndexWithFold foldFn indexPath@(MkPathI fp) =
  addNamespace "readIndexWithFold" $
    (readFile >=> runFold (pure mempty) . decode) fp
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
    runFold _ (Nil (Just err) rest) = do
      $(logError) ("Error end of stream: " <> T.pack err)
      throwCS $
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
    runFold _ (Nil _ rest) = do
      $(logError) ("Unconsumed input: " <> lbsToTxt rest)
      throwCS $
        MkExceptionI @ReadIndex
          (indexPath, "Unconsumed input: " <> lbsToStr rest)
    -- Encountered an error.
    runFold _ (Cons (Left err) _) = do
      $(logError) ("Error reading stream: " <> T.pack err)
      throwCS $ MkExceptionI @ReadIndex (indexPath, err)
    -- Inductive case, run fold and recurse
    runFold macc (Cons (Right x) rest) = runFold (foldFn macc x) rest

    lbsToStr = Char8.unpack . BSL.toStrict
    lbsToTxt = TEnc.decodeUtf8With TEncError.lenientDecode . BSL.toStrict

-- | Verifies that the 'PathData'\'s @fileName@ does not exist in the
-- hashmap.
--
-- @since 0.1
throwIfDuplicates ::
  (HasCallStack, MonadIO m) =>
  PathI TrashIndex ->
  HashMap (PathI TrashName) PathData ->
  PathData ->
  m ()
throwIfDuplicates indexPath trashMap pd =
  when (fileName `HMap.member` trashMap) $
    throwCS $
      MkExceptionI @DuplicateIndexPath (indexPath, fileName)
  where
    fileName = pd ^. #fileName

-- | Verifies that the 'PathData'\'s @fileName@ actually exists.
--
-- @since 0.1
throwIfTrashNonExtant ::
  ( FileSystemReader m,
    HasCallStack,
    MonadIO m
  ) =>
  PathI TrashHome ->
  PathData ->
  m ()
throwIfTrashNonExtant trashHome pd = do
  exists <- PathData.trashPathExists trashHome pd
  unless exists $
    throwCS $
      MkExceptionI @TrashPathNotFound (trashHome, filePath)
  where
    filePath = pd ^. #fileName

-- | Appends the path data to the trash index. The header is not included.
--
-- @since 0.1
appendIndex :: FileSystemWriter m => PathI TrashIndex -> Index -> m ()
appendIndex (MkPathI indexPath) =
  appendFile indexPath
    . BSL.toStrict
    . Csv.encode
    . HMap.elems
    . view #unIndex

-- | Writes the path data to the trash index, overwriting the index if it
-- exists. The header is included.
--
-- @since 0.1
writeIndex :: FileSystemWriter m => PathI TrashIndex -> Index -> m ()
writeIndex (MkPathI indexPath) =
  writeFile indexPath
    . BSL.toStrict
    . Csv.encodeDefaultOrderedByName
    . HMap.elems
    . view #unIndex
