{-# LANGUAGE TemplateHaskell #-}
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
    ArbitraryE (..),

    -- ** Aggregating
    Exceptions (..),

    -- * Functions
    withStackTracing,

    -- * Optics
    _MkPathNotFoundE,
    _MkRenameDuplicateE,
    _MkReadIndexE,
    _MkDuplicateIndexPathE,
    _MkTrashPathNotFoundE,
    _MkRestoreCollisionE,
    _MkIndexSizeMismatchE,
    _MkTomlDecodeE,
    _MkArbitraryE,
  )
where

import Data.Text qualified as T
import SafeRm.Data.Paths
  ( PathI,
    PathIndex (OriginalPath, TrashHome, TrashIndex, TrashName),
  )
import SafeRm.Effects.MonadCallStack (MonadCallStack, throwCallStack)
import SafeRm.Prelude
import TOML (TOMLError, renderTOMLError)

appendPrettyCs :: CallStack -> String
appendPrettyCs cs =
  mconcat
    [ "\n\n",
      prettyCallStack cs
    ]

-- | Path is not found.
--
-- @since 0.1
data PathNotFoundE = MkPathNotFoundE !FilePath !CallStack
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
makePrisms ''PathNotFoundE

-- | @since 0.1
instance Exception PathNotFoundE where
  displayException (MkPathNotFoundE f cs) =
    mconcat
      [ "Path not found: ",
        f,
        appendPrettyCs cs
      ]

-- | Could not rename file due to duplicate names.
--
-- @since 0.1
data RenameDuplicateE = MkRenameDuplicateE !(PathI TrashName) !CallStack
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
makePrisms ''RenameDuplicateE

-- | @since 0.1
instance Exception RenameDuplicateE where
  displayException (MkRenameDuplicateE n cs) =
    mconcat
      [ "Failed renaming duplicate file: ",
        n ^. #unPathI,
        appendPrettyCs cs
      ]

-- | Error reading the index.
--
-- @since 0.1
data ReadIndexE = MkReadIndexE !(PathI TrashIndex) !String !CallStack
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
makePrisms ''ReadIndexE

-- | @since 0.1
instance Exception ReadIndexE where
  displayException (MkReadIndexE f err cs) =
    mconcat
      [ "Error reading index at '",
        f ^. #unPathI,
        "': ",
        err,
        appendPrettyCs cs
      ]

-- | Duplicate paths found in index file.
--
-- @since 0.1
data DuplicateIndexPathE
  = MkDuplicateIndexPathE
      !(PathI TrashIndex)
      !(PathI TrashName)
      !CallStack
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
makePrisms ''DuplicateIndexPathE

-- | @since 0.1
instance Exception DuplicateIndexPathE where
  displayException (MkDuplicateIndexPathE f n cs) =
    mconcat
      [ "Trash paths should be unique, but found multiple entries in the ",
        "trash index '",
        f ^. #unPathI,
        "' for the following path: ",
        n ^. #unPathI,
        appendPrettyCs cs
      ]

-- | Path not found in trash error.
--
-- @since 0.1
data TrashPathNotFoundE
  = MkTrashPathNotFoundE
      !(PathI TrashHome)
      !(PathI TrashName)
      !CallStack
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
makePrisms ''TrashPathNotFoundE

-- | @since 0.1
instance Exception TrashPathNotFoundE where
  displayException (MkTrashPathNotFoundE f n cs) =
    mconcat
      [ "The path '",
        n ^. #unPathI,
        "' was not found in the trash directory '",
        f ^. #unPathI,
        "' despite being listed in the trash index. This can be fixed by ",
        "manually deleting the entry from the index or deleting everything ",
        "(i.e. sr e).",
        appendPrettyCs cs
      ]

-- | Collision with existing file when attempting a restore.
--
-- @since 0.1
data RestoreCollisionE
  = MkRestoreCollisionE
      !(PathI TrashName)
      !(PathI OriginalPath)
      !CallStack
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
makePrisms ''RestoreCollisionE

-- | @since 0.1
instance Exception RestoreCollisionE where
  displayException (MkRestoreCollisionE n o cs) =
    mconcat
      [ "Cannot restore the trash file '",
        n ^. #unPathI,
        "' as one exists at the original location: ",
        o ^. #unPathI,
        appendPrettyCs cs
      ]

-- | Index size did not match the trash directory.
--
-- @since 0.1
data IndexSizeMismatchE
  = MkIndexSizeMismatchE
      !(PathI TrashHome)
      !Int
      !Int
      !CallStack
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
makePrisms ''IndexSizeMismatchE

-- | @since 0.1
instance Exception IndexSizeMismatchE where
  displayException (MkIndexSizeMismatchE f numEntries numIndex cs) =
    mconcat
      [ "Size mismatch between index size (",
        show numIndex,
        ") and number of entries (",
        show numEntries,
        ") in trash: ",
        f ^. #unPathI,
        appendPrettyCs cs
      ]

-- | Error decoding TOML file.
--
-- @since 0.1
data TomlDecodeE = MkTomlDecodeE !TOMLError !CallStack
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
makePrisms ''TomlDecodeE

-- | @since 0.1
instance Exception TomlDecodeE where
  displayException (MkTomlDecodeE err cs) =
    mconcat
      [ "Error decoding toml: ",
        T.unpack (renderTOMLError err),
        appendPrettyCs cs
      ]

-- | Arbitrary exception. 'SomeException' with 'CallStack'.
--
-- @since 0.1
data ArbitraryE = MkArbitraryE !SomeException !CallStack
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
makePrisms ''ArbitraryE

-- | @since 0.1
instance Exception ArbitraryE where
  displayException (MkArbitraryE e cs) =
    mconcat
      [ "Exception: ",
        displayException e,
        appendPrettyCs cs
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
      foldExs acc ex = ("\n\n- " <> displayException ex) <> acc

-- | Tries the monadic action. If an exception is encountered, it is wrapped
-- in an 'ArbitraryE' and rethrown.
--
-- @since 0.1
withStackTracing ::
  ( HasCallStack,
    MonadCallStack m,
    MonadUnliftIO m
  ) =>
  m a ->
  m a
withStackTracing =
  tryAny >=> \case
    Left ex -> throwCallStack @_ @ArbitraryE (MkArbitraryE ex)
    Right x -> pure x
