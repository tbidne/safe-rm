-- | Provides types.
--
-- @since 0.1
module Del.Data.Statistics
  ( Statistics (..),
    getStats,
  )
where

import Data.Bytes (Bytes (MkBytes), Size (B), SomeSize)
import Data.Bytes qualified as Bytes
import Data.Bytes.Formatting (FloatingFormatter (MkFloatingFormatter))
import Del.Data.Paths (PathI (..), PathIndex (..))
import Del.Exceptions (PathNotFoundError (..))
import Del.Prelude
import System.Directory qualified as Dir

-- | Holds trash statistics.
--
-- @since 0.1
data Statistics = MkStatistics
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
instance Pretty Statistics where
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

-- | Returns stats on the trash directory.
--
-- @since 0.1
getStats :: PathI TrashHome -> IO Statistics
getStats (MkPathI trashHome) = do
  -- TODO: This _should_ be the same as the index length (corresponds exactly
  -- to top-level paths except .index.csv). We do this instead of parsing
  -- the entire index for performance.
  --
  -- We may want to actually verify this invariant here, failing if there is
  -- a mismatch.
  numEntries <- (\xs -> length xs - 1) <$> Dir.listDirectory trashHome
  allFiles <- getAllFiles trashHome
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
