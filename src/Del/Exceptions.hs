-- | Provides exceptions used by Del.
--
-- @since 0.1
module Del.Exceptions
  ( PathNotFoundError (..),
    RenameDuplicateError (..),
    ReadIndexError (..),
    DuplicateIndexPathsError (..),
    TrashPathNotFoundError (..),
    PathExistsError (..),
  )
where

import Control.DeepSeq (NFData)
import Control.Exception (Exception (displayException))
import GHC.Generics (Generic)

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
newtype RenameDuplicateError = MkRenameDuplicateError FilePath
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
  displayException (MkRenameDuplicateError fp) =
    "Failed renaming duplicate file: " <> fp

-- | Error when attempting to read the index.
--
-- @since 0.1
newtype ReadIndexError = MkReadIndexError String
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
  displayException (MkReadIndexError err) = "Error reading index: " <> err

-- | Duplicate trash paths found.
--
-- @since 0.1
newtype DuplicateIndexPathsError = MkDuplicateIndexPathsError FilePath
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
  displayException (MkDuplicateIndexPathsError fp) =
    mconcat
      [ "Trash paths should be unique, but found multiple entries for the ",
        "following path: ",
        fp
      ]

-- | Path not found in trash.
--
-- @since 0.1
newtype TrashPathNotFoundError = MkTrashPathsNotFoundError FilePath
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
  displayException (MkTrashPathsNotFoundError fp) =
    mconcat
      [ "The following path was listed in the trash index but was not found ",
        "in the trash directory: ",
        fp
      ]

-- | Path already exists.
--
-- @since 0.1
newtype PathExistsError = MkPathExistsError FilePath
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
instance Exception PathExistsError where
  displayException (MkPathExistsError fp) =
    "File already exists at the original path: " <> fp
