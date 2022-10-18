{-# LANGUAGE UndecidableInstances #-}

-- | Provides exceptions used by SafeRm.
--
-- @since 0.1
module SafeRm.Exceptions
  ( -- * Types
    ExceptionI (..),
    ExceptionIndex (..),
    ExceptionF,

    -- * Functions
    wrapCS,
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
import SafeRm.Effects.MonadCallStack (MonadCallStack, throwCS)
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
  | -- | An arbitrary exception.
    --
    -- @since 0.1
    OneException
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
  ExceptionF OneException = SomeException

-- | Indexed 'Exception' for simplifying the interface for throwing different
-- types of exceptions.
--
-- @since 0.1
type ExceptionI :: ExceptionIndex -> Type
data ExceptionI i = MkExceptionI (ExceptionF i) CallStack

-- | @since 0.1
instance (Show (ExceptionF i), Typeable i) => Show (ExceptionI i) where
  -- NOTE: not derived so we can include the index.
  showsPrec i (MkExceptionI e cs) =
    Show.showParen
      (i >= Show.appPrec1)
      ( Show.showString baseName
          . Show.showsPrec Show.appPrec1 e
          . Show.showsPrec Show.appPrec1 cs
      )
    where
      baseName = "MkExceptionI (" <> show rep <> ") "
      rep = typeOf @(Proxy i) Proxy

-- | @since 0.1
instance Exception (ExceptionI PathNotFound) where
  displayException (MkExceptionI fp cs) =
    mconcat
      [ "Path not found: ",
        fp,
        "\n",
        prettyCallStack cs
      ]

-- | @since 0.1
instance Exception (ExceptionI RenameDuplicate) where
  displayException (MkExceptionI (MkPathI fp) cs) =
    mconcat
      [ "Failed renaming duplicate file: ",
        fp,
        "\n",
        prettyCallStack cs
      ]

-- | @since 0.1
instance Exception (ExceptionI ReadIndex) where
  displayException (MkExceptionI (MkPathI indexPath, err) cs) =
    mconcat
      [ "Error reading index at '",
        indexPath,
        "': ",
        err,
        "\n",
        prettyCallStack cs
      ]

-- | @since 0.1
instance Exception (ExceptionI DuplicateIndexPath) where
  displayException
    (MkExceptionI (MkPathI trashIndex, MkPathI dupName) cs) =
      mconcat
        [ "Trash paths should be unique, but found multiple entries in the ",
          "trash index '",
          trashIndex,
          "' for the following path: ",
          dupName,
          "\n",
          prettyCallStack cs
        ]

-- | @since 0.1
instance Exception (ExceptionI TrashPathNotFound) where
  displayException
    (MkExceptionI (MkPathI trashHome, MkPathI notFound) cs) =
      mconcat
        [ "The path '",
          notFound,
          "' was not found in the trash directory '",
          trashHome,
          "' despite being listed in the trash index. This can be fixed by ",
          "manually deleting the entry from the index or deleting everything ",
          "(i.e. sr e).",
          "\n",
          prettyCallStack cs
        ]

-- | @since 0.1
instance Exception (ExceptionI RestoreCollision) where
  displayException (MkExceptionI (MkPathI trashName, MkPathI originalPath) cs) =
    mconcat
      [ "Cannot restore the trash file '",
        trashName,
        "' as one exists at the original location: ",
        originalPath,
        "\n",
        prettyCallStack cs
      ]

-- | @since 0.1
instance Exception (ExceptionI TrashIndexSizeMismatch) where
  displayException (MkExceptionI (MkPathI trashHome, dirSize, indexSize) cs) =
    mconcat
      [ "Size mismatch between index size (",
        show indexSize,
        ") and number of entries (",
        show dirSize,
        ") in trash: ",
        trashHome,
        "\n",
        prettyCallStack cs
      ]

-- | @since 0.1
instance Exception (ExceptionI TomlDecode) where
  displayException (MkExceptionI tomlError cs) =
    mconcat
      [ "Error decoding toml: ",
        T.unpack (renderTOMLError tomlError),
        "\n",
        prettyCallStack cs
      ]

-- | @since 0.1
instance Exception (ExceptionI OneException) where
  displayException (MkExceptionI ex cs) =
    mconcat
      [ "Exception: ",
        displayException ex,
        "\n",
        prettyCallStack cs
      ]

-- | @since 0.1
instance Exception (ExceptionI SomeExceptions) where
  displayException (MkExceptionI xs cs) =
    mconcat
      [ "Encountered exception(s)\n\n",
        foldl' foldExs "" xs,
        "\n\n",
        prettyCallStack cs
      ]
    where
      foldExs acc ex = ("- " <> displayException ex <> "\n") <> acc

-- | Tries the monadic action. If an exception is encountered, it is wrapped
-- in an 'ExceptionI' 'OneException' and rethrown.
--
-- @since 0.1
wrapCS ::
  ( HasCallStack,
    MonadCallStack m,
    MonadUnliftIO m
  ) =>
  m a ->
  m a
wrapCS =
  try >=> \case
    Left ex -> throwCS @_ @(ExceptionI OneException) (MkExceptionI ex)
    Right x -> pure x
