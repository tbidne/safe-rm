{-# LANGUAGE UndecidableInstances #-}

-- | Provides exceptions used by Del.
--
-- @since 0.1
module Del.Exceptions
  ( ExceptionI (..),
    _MkExceptionI,
    ExceptionIndex (..),
    ExceptionF,
  )
where

import Del.Data.Paths (PathI (..), PathIndex (..))
import Del.Prelude

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
  ExceptionF RestoreCollision = (PathI TrashName, PathI OriginalName)
  ExceptionF TrashIndexSizeMismatch = (PathI TrashHome, Int, Int)

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
deriving stock instance Show (ExceptionF i) => Show (ExceptionI i)

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
  -- TODO: mention commands for fixing these (e.g. empty, new 'fix' command)
  displayException
    (MkExceptionI (MkPathI trashHome, MkPathI notFound)) =
      mconcat
        [ "The path '",
          notFound,
          "' was not found in the trash directory '",
          trashHome,
          "' despite being listed in the trash index."
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
