-- | Provides metadata functionality.
--
-- @since 0.1
module SafeRm.Data.Metadata
  ( Metadata (..),
    getMetadata,
  )
where

import Data.Bytes (Bytes (MkBytes), Size (B), SomeSize)
import Data.Bytes qualified as Bytes
import Data.Bytes.Formatting (FloatingFormatter (MkFloatingFormatter))
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Numeric.Algebra (AMonoid (zero), ASemigroup ((.+.)))
import SafeRm.Data.Index (Index (unIndex))
import SafeRm.Data.Index qualified as Index
import SafeRm.Data.Paths (PathI (MkPathI))
import SafeRm.Effects.Logger (Logger (addNamespace))
import SafeRm.Effects.Logger qualified as Logger
import SafeRm.Env (HasTrashHome, getTrashPaths)
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
getMetadata ::
  ( HasTrashHome env,
    Logger m,
    MonadReader env m,
    MonadIO m
  ) =>
  m Metadata
getMetadata = addNamespace "getMetadata" $ do
  (trashHome@(MkPathI th), trashIndex) <- asks getTrashPaths
  Logger.logDebug ("Trash home: " <> T.pack th)
  index <- view #unIndex <$> Index.readIndex trashIndex
  let numIndex = Map.size index
  Logger.logDebug ("Index size: " <> showt numIndex)
  numEntries <- (\xs -> length xs - 1) <$> Dir.listDirectory th
  Logger.logDebug ("Num entries: " <> showt numEntries)
  allFiles <- getAllFiles th
  allSizes <- toDouble <$> foldl' sumFileSizes (pure 0) allFiles
  let numFiles = length allFiles - 1
      normalized = Bytes.normalize (MkBytes @B allSizes)

  Logger.logDebug ("Num all files: " <> showt numFiles)
  Logger.logDebug ("Total size: " <> showt normalized)

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
      sz <- Dir.getFileSize f
      pure $ acc + sz
    toDouble :: Integer -> Double
    toDouble = fromIntegral
    toNat :: Int -> Natural
    toNat = fromIntegral

getAllFiles :: (Logger m, MonadIO m) => FilePath -> m [FilePath]
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
