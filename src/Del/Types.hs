{-# LANGUAGE CPP #-}

module Del.Types
  ( PathType (..),
    PathData (..),
    Index (..),
  )
where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Csv (FromField, FromRecord, ToField, ToRecord)
import Data.Csv qualified as Csv
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as Map
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
#if !MIN_VERSION_prettyprinter(1, 7, 1)
import Data.Text.Prettyprint.Doc (Pretty (pretty), (<+>))
import Data.Text.Prettyprint.Doc qualified as Pretty
#else
import Prettyprinter (Pretty (pretty), (<+>))
import Prettyprinter qualified as Pretty
#endif
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Text.Encoding.Error qualified as TEncError

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
  { -- | The path to be used in the trash directory.
    --
    -- @since 0.1
    trashPath :: !FilePath,
    -- | The original path on the file system.
    --
    -- @since 0.1
    originalPath :: !FilePath,
    -- | The type of the path.
    --
    -- @since 0.1
    pathType :: !PathType
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
instance Pretty PathData where
  pretty MkPathData {trashPath, originalPath} = Pretty.vsep strs
    where
      strs =
        [ "-" <+> pretty trashPath,
          Pretty.indent 2 (pretty originalPath)
        ]

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
    Pretty.vsep
      . fmap pretty
      . Map.elems
      . unIndex

-- | Converts UTF8 'ByteString' to 'String'. Decoding is lenient.
--
-- @since 0.1
bsToStr :: ByteString -> String
bsToStr = T.unpack . TEnc.decodeUtf8With TEncError.lenientDecode
