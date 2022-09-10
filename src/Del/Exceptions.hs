-- | Provides exceptions used by Del.
--
-- @since 0.1
module Del.Exceptions
  ( PathNotFoundError (..),
    RenameDuplicateError (..),
    ReadIndexError (..),
    PathNotInIndexError (..),
    DuplicateIndexPathsError (..),
    TrashPathNotFoundError (..),
    PathExistsError (..),
  )
where

import Control.DeepSeq (NFData)
import Control.Exception (Exception (displayException))
import Data.Vector (Vector)
import Data.Vector qualified as V
import Del.Types (PathData (..))
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
  displayException (MkRenameDuplicateError fp) = "Failed renaming duplicate file: " <> fp

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

-- | Path not found in index.
--
-- @since 0.1
newtype PathNotInIndexError = MkPathNotInIndexError FilePath
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
instance Exception PathNotInIndexError where
  displayException (MkPathNotInIndexError fp) =
    "Path not found in trash index: " <> fp

-- | Duplicate trash paths found.
--
-- @since 0.1
newtype DuplicateIndexPathsError = MkDuplicateIndexPathsError (Vector PathData)
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
  displayException (MkDuplicateIndexPathsError matches) =
    mconcat
      [ "Wanted a unique match in the index, found ",
        show $ V.length matches,
        ": ",
        showVec matches
      ]
    where
      showVec :: Vector PathData -> String
      showVec = V.foldl' f ""
      f acc pd = "\n - " <> showPd pd <> acc
      showPd MkPathData {trashPath, originalPath} =
        mconcat
          [ "trash path: ",
            trashPath,
            "\n   original path: ",
            originalPath,
            "\n"
          ]

-- | Path not found in trash.
--
-- @since 0.1
newtype TrashPathNotFoundError = MkTrashPathNotFoundError FilePath
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
  displayException (MkTrashPathNotFoundError fp) = "Path not found in trash: " <> fp

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
