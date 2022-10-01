{-# LANGUAGE UndecidableInstances #-}

-- | Provides exceptions used by SafeRm.
--
-- @since 0.1
module SafeRm.Exceptions
  ( ExceptionI (..),
    _MkExceptionI,
    ExceptionIndex (..),
    ExceptionF,
  )
where

import Data.Text qualified as T
import Data.Typeable (Typeable, typeOf)
import GHC.Show (Show (showsPrec))
import GHC.Show qualified as Show
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex (OriginalPath, TrashHome, TrashIndex, TrashName),
  )
import SafeRm.Prelude
import TOML (TOMLError, renderTOMLError)

-- | Types of exceptions thrown.
--
-- @since 0.1
data ExceptionIndex
  = -- | Path not found.
    --
    -- @since 0.1
    PathNotFound
  | -- | Renaming duplicate filename failed.
    --
    -- @since 0.1
    RenameDuplicate
  | -- | Reading index failed.
    --
    -- @since 0.1
    ReadIndex
  | -- | Duplicate index paths found.
    --
    -- @since 0.1
    DuplicateIndexPath
  | -- | Trash path not found.
    --
    -- @since 0.1
    TrashPathNotFound
  | -- | Collision when restoring original path.
    --
    -- @since 0.1
    RestoreCollision
  | -- | The size of the trash index and number of entries does not match.
    --
    -- @since 0.1
    TrashIndexSizeMismatch
  | -- | Error reading toml configuration.
    --
    -- @since 0.1
    TomlDecode
  | -- | Collects multiple arbitrary exceptions.
    --
    -- @since 0.1
    SomeExceptions
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

-- | Associates an 'ExceptionIndex' to its exception data type.
--
-- @since 0.1
type ExceptionF :: ExceptionIndex -> Type
type family ExceptionF e where
  ExceptionF PathNotFound = FilePath
  ExceptionF RenameDuplicate = PathI TrashName
  ExceptionF ReadIndex = (PathI TrashIndex, String)
  ExceptionF DuplicateIndexPath = (PathI TrashIndex, PathI TrashName)
  ExceptionF TrashPathNotFound = (PathI TrashHome, PathI TrashName)
  ExceptionF RestoreCollision = (PathI TrashName, PathI OriginalPath)
  ExceptionF TrashIndexSizeMismatch = (PathI TrashHome, Int, Int)
  ExceptionF TomlDecode = TOMLError
  ExceptionF SomeExceptions = NonEmpty SomeException

-- | Indexed 'Exception' for simplifying the interface for throwing different
-- types of exceptions.
--
-- @since 0.1
type ExceptionI :: ExceptionIndex -> Type
newtype ExceptionI i = MkExceptionI (ExceptionF i)
  deriving stock
    ( -- | @since 0.1
      Generic
    )

-- | @since 0.1
deriving stock instance Eq (ExceptionF i) => Eq (ExceptionI i)

-- | @since 0.1
instance (Show (ExceptionF i), Typeable i) => Show (ExceptionI i) where
  -- NOTE: not derived so we can include the index.
  showsPrec i (MkExceptionI e) =
    Show.showParen
      (i >= Show.appPrec1)
      (Show.showString baseName . Show.showsPrec Show.appPrec1 e)
    where
      baseName = "MkExceptionI (" <> show rep <> ") "
      rep = typeOf @(Proxy i) Proxy

-- | @since 0.1
deriving anyclass instance NFData (ExceptionF i) => NFData (ExceptionI i)

-- | @since 0.1
_MkExceptionI :: Iso' (ExceptionI i) (ExceptionF i)
_MkExceptionI = iso (\(MkExceptionI e) -> e) MkExceptionI

-- | @since 0.1
instance Exception (ExceptionI PathNotFound) where
  displayException (MkExceptionI fp) = "Path not found: " <> fp

-- | @since 0.1
instance Exception (ExceptionI RenameDuplicate) where
  displayException (MkExceptionI (MkPathI fp)) =
    "Failed renaming duplicate file: " <> fp

-- | @since 0.1
instance Exception (ExceptionI ReadIndex) where
  displayException (MkExceptionI (MkPathI indexPath, err)) =
    mconcat
      [ "Error reading index at '",
        indexPath,
        "': ",
        err
      ]

-- | @since 0.1
instance Exception (ExceptionI DuplicateIndexPath) where
  displayException
    (MkExceptionI (MkPathI trashIndex, MkPathI dupName)) =
      mconcat
        [ "Trash paths should be unique, but found multiple entries in the ",
          "trash index '",
          trashIndex,
          "' for the following path: ",
          dupName
        ]

-- | @since 0.1
instance Exception (ExceptionI TrashPathNotFound) where
  displayException
    (MkExceptionI (MkPathI trashHome, MkPathI notFound)) =
      mconcat
        [ "The path '",
          notFound,
          "' was not found in the trash directory '",
          trashHome,
          "' despite being listed in the trash index. This can be fixed by ",
          "manually deleting the entry from the index or deleting everything ",
          "(i.e. sr e)."
        ]

-- | @since 0.1
instance Exception (ExceptionI RestoreCollision) where
  displayException (MkExceptionI (MkPathI trashName, MkPathI originalPath)) =
    mconcat
      [ "Cannot restore the trash file '",
        trashName,
        "' as one exists at the original location: ",
        originalPath
      ]

-- | @since 0.1
instance Exception (ExceptionI TrashIndexSizeMismatch) where
  displayException (MkExceptionI (MkPathI trashHome, dirSize, indexSize)) =
    mconcat
      [ "Size mismatch between index size (",
        show indexSize,
        ") and number of entries (",
        show dirSize,
        ") in trash: ",
        trashHome
      ]

-- | @since 0.1
instance Exception (ExceptionI TomlDecode) where
  displayException (MkExceptionI tomlError) =
    mconcat
      [ "Error decoding toml: ",
        T.unpack (renderTOMLError tomlError)
      ]

-- | @since 0.1
instance Exception (ExceptionI SomeExceptions) where
  displayException (MkExceptionI xs) =
    mconcat
      [ "Encountered exception(s)\n",
        foldl' foldExs "" xs
      ]
    where
      foldExs acc ex = ("- " <> displayException ex <> "\n") <> acc
