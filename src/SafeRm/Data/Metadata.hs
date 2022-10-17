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
import Numeric.Literal.Rational (FromRational (afromRational))
import SafeRm.Data.Index qualified as Index
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex (TrashHome, TrashIndex, TrashLog),
  )
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
    -- | Size of the log file.
    --
    -- @since 0.1
    logSize :: !(SomeSize Double),
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
  MkMetadata a b c d <> MkMetadata a' b' c' d' =
    MkMetadata (a + a') (b + b') (c .+. c') (d .+. d')

-- | @since 0.1
instance Monoid Metadata where
  mempty = MkMetadata 0 0 zero zero

-- | @since 0.1
instance Pretty Metadata where
  pretty stats = vsep strs <+> line
    where
      strs =
        [ "Entries:     " <+> pretty (stats ^. #numEntries),
          "Total Files: " <+> pretty (stats ^. #numFiles),
          "Log size:    " <+> pretty (formatSz $ stats ^. #logSize),
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
  (PathI TrashHome, PathI TrashIndex, PathI TrashLog) ->
  m Metadata
toMetadata (trashHome@(MkPathI th), trashIndex, trashLog) =
  addNamespace "toMetadata" $ do
    -- Index size
    index <- view #unIndex <$> Index.readIndex trashIndex
    let numIndex = Map.size index
    $(logDebug) ("Index size: " <> showt numIndex)

    -- Num entries
    numEntries <- (\xs -> length xs - 1) <$> Dir.listDirectory th
    $(logDebug) ("Num entries: " <> showt numEntries)

    -- Log size
    let logPath = trashLog ^. #unPathI
    logExists <- Dir.doesFileExist logPath
    logSize <-
      if logExists
        then do
          logSize' <-
            Bytes.normalize . toDouble <$> getFileSize logPath
          $(logDebug) ("Log size: " <> showt logSize')
          pure logSize'
        else do
          $(logDebug) "Log does not exist"
          pure (afromRational 0)

    -- Summed size
    allFiles <- getAllFiles th
    allSizes <- toDouble <$> foldl' sumFileSizes (pure zero) allFiles
    let numFiles = length allFiles - 1
        size = Bytes.normalize allSizes

    $(logDebug) ("Num all files: " <> showt numFiles)
    $(logDebug) ("Total size: " <> showt size)

    -- NOTE: Verify that sizes are the same. Because reading the index verifies
    -- that there are no duplicate entries and each entry corresponds to a real
    -- trash path, this guarantees that the index exactly corresponds to the
    -- trash state.
    when (numEntries /= numIndex) $
      throwCS $
        MkExceptionI @TrashIndexSizeMismatch (trashHome, numFiles, numIndex)

    pure $
      MkMetadata
        { numEntries = toNat numEntries,
          numFiles = toNat numFiles,
          logSize,
          size
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
        False -> throwCS $ MkExceptionI @PathNotFound fp
