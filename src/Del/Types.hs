{-# LANGUAGE OverloadedLists #-}

-- | Provides types.
--
-- @since 0.1
module Del.Types
  ( PathType (..),
    PathData (..),
    Index (..),
    Statistics (..),
  )
where

import Data.Bytes (SomeSize)
import Data.Bytes qualified as Bytes
import Data.Bytes.Formatting (FloatingFormatter (MkFloatingFormatter))
import Data.Csv
  ( DefaultOrdered (headerOrder),
    FromField,
    FromNamedRecord,
    FromRecord,
    ToField,
    ToNamedRecord,
    ToRecord,
    (.:),
    (.=),
  )
import Data.Csv qualified as Csv
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Text.Encoding.Error qualified as TEncError
import Del.Prelude
import GHC.Exts (IsList (Item))

-- | Path type.
--
-- @since 0.1
data PathType
  = -- | File type.
    --
    -- @since 0.1
    PathTypeFile
  | -- | Directory type
    --
    -- @since 0.1
    PathTypeDirectory
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
      Hashable,
      -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance Pretty PathType where
  pretty PathTypeFile = "File"
  pretty PathTypeDirectory = "Directory"

-- | @since 0.1
instance FromField PathType where
  parseField s
    | s == "file" = pure PathTypeFile
    | s == "directory" = pure PathTypeDirectory
    | otherwise = fail $ "Expected 'file' or 'directory'. Received: " <> bsToStr s

-- | @since 0.1
instance ToField PathType where
  toField PathTypeFile = "file"
  toField PathTypeDirectory = "directory"

-- | Data for a path.
--
-- @since 0.1
data PathData = MkPathData
  { -- | The type of the path.
    --
    -- @since 0.1
    pathType :: !PathType,
    -- | The path to be used in the trash directory.
    --
    -- @since 0.1
    trashPath :: !FilePath,
    -- | The original path on the file system.
    --
    -- @since 0.1
    originalPath :: !FilePath
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
      FromRecord,
      -- | @since 0.1
      Hashable,
      -- | @since 0.1
      NFData,
      -- | @since 0.1
      ToRecord
    )

-- | @since 0.1
instance FromNamedRecord PathData where
  parseNamedRecord m =
    MkPathData
      <$> m .: "type"
      <*> m .: "trash"
      <*> m .: "original"

-- | @since 0.1
instance ToNamedRecord PathData where
  toNamedRecord pd = Map.fromList $ zipWith (flip ($)) headerNames labelFn
    where
      labelFn =
        [ \x -> x .= (pd ^. #pathType),
          \x -> x .= (pd ^. #trashPath),
          \x -> x .= (pd ^. #originalPath)
        ]

-- | @since 0.1
instance DefaultOrdered PathData where
  headerOrder _ = headerNames

-- | @since 0.1
instance Pretty PathData where
  pretty pd = vsep strs <+> line
    where
      strs = zipWith (flip ($)) headerNames labelFn
      labelFn =
        [ \x -> x <> ":    " <+> pretty (pd ^. #pathType),
          \x -> x <> ":   " <+> pretty (pd ^. #trashPath),
          \x -> x <> ":" <+> pretty (pd ^. #originalPath)
        ]

headerNames :: (IsList a, IsString (Item a)) => a
headerNames = ["type", "trash", "original"]

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
      . Map.elems
      . view #unIndex

-- | Converts UTF8 'ByteString' to 'String'. Decoding is lenient.
--
-- @since 0.1
bsToStr :: ByteString -> String
bsToStr = T.unpack . TEnc.decodeUtf8With TEncError.lenientDecode

-- | Holds trash statistics.
--
-- @since 0.1
data Statistics = MkStatistics
  { -- | Number of top level entires in the trash index. This should be the
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
