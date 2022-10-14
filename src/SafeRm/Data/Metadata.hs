{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides metadata functionality.
--
-- @since 0.1
module SafeRm.Data.Metadata
  ( Metadata (..),
    toMetadata,
  )
where

import Data.Bytes (Bytes, SomeSize)
import Data.Bytes qualified as Bytes
import Data.Bytes.Formatting (FloatingFormatter (MkFloatingFormatter))
import Data.HashMap.Strict qualified as Map
import Numeric.Algebra (AMonoid (zero), ASemigroup ((.+.)))
import SafeRm.Data.Index qualified as Index
import SafeRm.Data.Paths (PathI (MkPathI), PathIndex (TrashHome, TrashIndex))
import SafeRm.Effects.FileSystemReader (FileSystemReader (getFileSize))
import SafeRm.Effects.Logger (LoggerContext, addNamespace)
import SafeRm.Exceptions
  ( ExceptionI (MkExceptionI),
    ExceptionIndex (PathNotFound, TrashIndexSizeMismatch),
  )
import SafeRm.Prelude
import UnliftIO.Directory qualified as Dir

-- | Holds trash metadata.
--
-- @since 0.1
data Metadata = MkMetadata
  { -- | Number of top level entries in the trash index. This should be the
    -- same as the index length.
    --
    -- @since 0.1
    numEntries :: !Natural,
    -- | Number of total files in the trash.
    --
    -- @since 0.1
    numFiles :: !Natural,
    -- | Total size of the trash directory.
    --
    -- @since 0.1
    size :: !(SomeSize Double)
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

makeFieldLabelsNoPrefix ''Metadata

-- | @since 0.1
instance Semigroup Metadata where
  MkMetadata a b c <> MkMetadata a' b' c' =
    MkMetadata (a + a') (b + b') (c .+. c')

-- | @since 0.1
instance Monoid Metadata where
  mempty = MkMetadata 0 0 zero

-- | @since 0.1
instance Pretty Metadata where
  pretty stats = vsep strs <+> line
    where
      strs =
        [ "Entries:     " <+> pretty (stats ^. #numEntries),
          "Total Files: " <+> pretty (stats ^. #numFiles),
          "Size:        " <+> pretty (formatSz $ stats ^. #size)
        ]
      formatSz =
        Bytes.formatSized
          (MkFloatingFormatter (Just 2))
          Bytes.sizedFormatterUnix

-- | Returns metadata for the trash directory.
--
-- @since 0.1
toMetadata ::
  ( FileSystemReader m,
    LoggerContext m,
    MonadIO m
  ) =>
  (PathI TrashHome, PathI TrashIndex) ->
  m Metadata
toMetadata (trashHome@(MkPathI th), trashIndex) = addNamespace "toMetadata" $ do
  index <- view #unIndex <$> Index.readIndex trashIndex
  let numIndex = Map.size index
  $(logDebug) ("Index size: " <> showt numIndex)
  numEntries <- (\xs -> length xs - 1) <$> Dir.listDirectory th
  $(logDebug) ("Num entries: " <> showt numEntries)
  allFiles <- getAllFiles th
  allSizes <- toDouble <$> foldl' sumFileSizes (pure zero) allFiles
  let numFiles = length allFiles - 1
      normalized = Bytes.normalize allSizes

  $(logDebug) ("Num all files: " <> showt numFiles)
  $(logDebug) ("Total size: " <> showt normalized)

  -- NOTE: Verify that sizes are the same. Because reading the index verifies
  -- that there are no duplicate entries and each entry corresponds to a real
  -- trash path, this guarantees that the index exactly corresponds to the
  -- trash state.
  when (numEntries /= numIndex) $
    throwIO $
      MkExceptionI @TrashIndexSizeMismatch (trashHome, numFiles, numIndex)

  pure $
    MkMetadata
      { numEntries = toNat numEntries,
        numFiles = toNat numFiles,
        size = normalized
      }
  where
    sumFileSizes macc f = do
      !acc <- macc
      sz <- getFileSize f
      pure $ acc .+. sz
    toDouble :: Bytes s Natural -> Bytes s Double
    toDouble = fmap fromIntegral
    toNat :: Int -> Natural
    toNat = fromIntegral

getAllFiles :: MonadIO m => FilePath -> m [FilePath]
getAllFiles fp =
  Dir.doesFileExist fp >>= \case
    True -> pure [fp]
    False ->
      Dir.doesDirectoryExist fp >>= \case
        True ->
          Dir.listDirectory fp
            >>= fmap join
              . traverse (getAllFiles . (fp </>))
        False -> throwIO $ MkExceptionI @PathNotFound fp
