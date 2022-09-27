-- | Provides exceptions used by Del.
--
-- @since 0.1
module Del.Exceptions
  ( PathNotFoundError (..),
    RenameDuplicateError (..),
    ReadIndexError (..),
    DuplicateIndexPathsError (..),
    TrashPathNotFoundError (..),
    RestoreCollisionError (..),
  )
where

import Del.Data.Paths (PathI (..), PathIndex (..))
import Del.Prelude

-- | Error when searching for a path.
--
-- @since 0.1
newtype PathNotFoundError = MkPathNotFoundError FilePath
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
instance Exception PathNotFoundError where
  displayException (MkPathNotFoundError fp) = "Path not found: " <> fp

-- | Error when attempting to rename a duplicate path.
--
-- @since 0.1
newtype RenameDuplicateError = MkRenameDuplicateError (PathI TrashName)
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
instance Exception RenameDuplicateError where
  displayException (MkRenameDuplicateError (MkPathI fp)) =
    "Failed renaming duplicate file: " <> fp

-- | Error when attempting to read the index.
--
-- @since 0.1
newtype ReadIndexError = MkReadIndexError (PathI TrashIndex, String)
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
instance Exception ReadIndexError where
  displayException (MkReadIndexError (MkPathI indexPath, err)) =
    mconcat
      [ "Error reading index at <",
        indexPath,
        ">: ",
        err
      ]

-- | Duplicate trash paths found.
--
-- @since 0.1
newtype DuplicateIndexPathsError
  = MkDuplicateIndexPathsError (PathI TrashIndex, PathI TrashName)
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
instance Exception DuplicateIndexPathsError where
  displayException
    (MkDuplicateIndexPathsError (MkPathI trashIndex, MkPathI dupName)) =
      mconcat
        [ "Trash paths should be unique, but found multiple entries in the ",
          "trash index <",
          trashIndex,
          "> for the following path: ",
          dupName
        ]

-- | Path not found in trash.
--
-- @since 0.1
newtype TrashPathNotFoundError
  = MkTrashPathNotFoundError (PathI TrashHome, PathI TrashName)
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
instance Exception TrashPathNotFoundError where
  -- TODO: mention commands for fixing these (e.g. empty, new 'fix' command)
  displayException
    (MkTrashPathNotFoundError (MkPathI trashHome, MkPathI notFound)) =
      mconcat
        [ "The path <",
          notFound,
          "> was not found in the trash directory <",
          trashHome,
          "> despite being listed in the trash index."
        ]

-- | Path already exists.
--
-- @since 0.1
newtype RestoreCollisionError = MkRestoreCollisionError (PathI OriginalName)
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
instance Exception RestoreCollisionError where
  displayException (MkRestoreCollisionError (MkPathI fp)) =
    "Cannot restore the file as one exists at the original location: "
      <> fp
