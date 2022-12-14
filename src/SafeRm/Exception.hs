{-# LANGUAGE UndecidableInstances #-}

-- | Provides exceptions used by SafeRm.
--
-- @since 0.1
module SafeRm.Exception
  ( -- * Types

    -- ** Specific
    PathNotFoundE (..),
    RenameDuplicateE (..),
    ReadIndexE (..),
    DuplicateIndexPathE (..),
    TrashPathNotFoundE (..),
    RestoreCollisionE (..),
    IndexSizeMismatchE (..),
    TomlDecodeE (..),

    -- ** Aggregating
    Exceptions (..),
  )
where

import Data.Text qualified as T
import SafeRm.Data.Paths
  ( PathI,
    PathIndex (OriginalPath, TrashHome, TrashIndex, TrashName),
  )
import SafeRm.Prelude
import TOML (TOMLError, renderTOMLError)

-- | Path is not found.
--
-- @since 0.1
newtype PathNotFoundE = MkPathNotFoundE FilePath
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception PathNotFoundE where
  displayException (MkPathNotFoundE f) =
    mconcat
      [ "Path not found: ",
        f
      ]

-- | Could not rename file due to duplicate names.
--
-- @since 0.1
newtype RenameDuplicateE = MkRenameDuplicateE (PathI TrashName)
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception RenameDuplicateE where
  displayException (MkRenameDuplicateE n) =
    mconcat
      [ "Failed renaming duplicate file: ",
        n ^. #unPathI
      ]

-- | Error reading the index.
--
-- @since 0.1
data ReadIndexE = MkReadIndexE !(PathI TrashIndex) !String
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception ReadIndexE where
  displayException (MkReadIndexE f err) =
    mconcat
      [ "Error reading index at '",
        f ^. #unPathI,
        "': ",
        err
      ]

-- | Duplicate paths found in index file.
--
-- @since 0.1
data DuplicateIndexPathE
  = MkDuplicateIndexPathE
      !(PathI TrashIndex)
      !(PathI TrashName)
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception DuplicateIndexPathE where
  displayException (MkDuplicateIndexPathE f n) =
    mconcat
      [ "Trash paths should be unique, but found multiple entries in the ",
        "trash index '",
        f ^. #unPathI,
        "' for the following path: ",
        n ^. #unPathI
      ]

-- | Path not found in trash error.
--
-- @since 0.1
data TrashPathNotFoundE
  = MkTrashPathNotFoundE
      !(PathI TrashHome)
      !(PathI TrashName)
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception TrashPathNotFoundE where
  displayException (MkTrashPathNotFoundE f n) =
    mconcat
      [ "The path '",
        n ^. #unPathI,
        "' was not found in the trash directory '",
        f ^. #unPathI,
        "' despite being listed in the trash index. This can be fixed by ",
        "manually deleting the entry from the index or deleting everything ",
        "(i.e. sr e)."
      ]

-- | Collision with existing file when attempting a restore.
--
-- @since 0.1
data RestoreCollisionE
  = MkRestoreCollisionE
      !(PathI TrashName)
      !(PathI OriginalPath)
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception RestoreCollisionE where
  displayException (MkRestoreCollisionE n o) =
    mconcat
      [ "Cannot restore the trash file '",
        n ^. #unPathI,
        "' as one exists at the original location: ",
        o ^. #unPathI
      ]

-- | Index size did not match the trash directory.
--
-- @since 0.1
data IndexSizeMismatchE
  = MkIndexSizeMismatchE
      !(PathI TrashHome)
      !Int
      !Int
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception IndexSizeMismatchE where
  displayException (MkIndexSizeMismatchE f numEntries numIndex) =
    mconcat
      [ "Size mismatch between index size (",
        show numIndex,
        ") and number of entries (",
        show numEntries,
        ") in trash: ",
        f ^. #unPathI
      ]

-- | Error decoding TOML file.
--
-- @since 0.1
newtype TomlDecodeE = MkTomlDecodeE TOMLError
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception TomlDecodeE where
  displayException (MkTomlDecodeE err) =
    mconcat
      [ "Error decoding toml: ",
        T.unpack (renderTOMLError err)
      ]

-- | Aggregates multiple exceptions.
--
-- @since 0.1
newtype Exceptions = MkExceptions
  { -- | @since 0.1
    unExceptions :: NonEmpty SomeException
  }
  deriving stock
    ( -- | @since 0.1
      Show
    )
  deriving
    ( -- | @since 0.1
      Semigroup
    )
    via NonEmpty SomeException

-- | @since 0.1
instance Exception Exceptions where
  displayException (MkExceptions es) =
    mconcat
      [ "Exceptions:",
        foldl' foldExs "" es
      ]
    where
      foldExs acc ex = ("\n\n- " <> displayCallStack ex) <> acc
