{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- TODO: Rename

-- | Provides exceptions used by SafeRm.
--
-- @since 0.1
module SafeRm.Exceptions
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
    displayTraceIf,
    displayTrace,

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
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as TLB
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex (OriginalPath, TrashHome, TrashIndex, TrashName),
  )
import SafeRm.Effects.MonadCallStack (MonadCallStack, throwCallStack)
import SafeRm.Prelude
import TOML (TOMLError, renderTOMLError)

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
  displayException (MkPathNotFoundE fp _) =
    mconcat
      [ "Path not found: ",
        fp
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
instance Exception RenameDuplicateE where
  displayException (MkRenameDuplicateE (MkPathI fp) _) =
    mconcat
      [ "Failed renaming duplicate file: ",
        fp
      ]

-- | @since 0.1
makePrisms ''RenameDuplicateE

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
  displayException (MkReadIndexE (MkPathI indexPath) err _) =
    mconcat
      [ "Error reading index at '",
        indexPath,
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
      !CallStack
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
makePrisms ''DuplicateIndexPathE

-- | @since 0.1
instance Exception DuplicateIndexPathE where
  displayException
    (MkDuplicateIndexPathE (MkPathI trashIndex) (MkPathI dupName) _) =
      mconcat
        [ "Trash paths should be unique, but found multiple entries in the ",
          "trash index '",
          trashIndex,
          "' for the following path: ",
          dupName
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
  displayException
    (MkTrashPathNotFoundE (MkPathI trashHome) (MkPathI notFound) _) =
      mconcat
        [ "The path '",
          notFound,
          "' was not found in the trash directory '",
          trashHome,
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
      !CallStack
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
makePrisms ''RestoreCollisionE

-- | @since 0.1
instance Exception RestoreCollisionE where
  displayException
    ( MkRestoreCollisionE
        (MkPathI trashName)
        (MkPathI originalPath)
        _
      ) =
      mconcat
        [ "Cannot restore the trash file '",
          trashName,
          "' as one exists at the original location: ",
          originalPath
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
  displayException
    ( MkIndexSizeMismatchE
        (MkPathI trashHome)
        dirSize
        indexSize
        _
      ) =
      mconcat
        [ "Size mismatch between index size (",
          show indexSize,
          ") and number of entries (",
          show dirSize,
          ") in trash: ",
          trashHome
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
  displayException (MkTomlDecodeE tomlError _) =
    mconcat
      [ "Error decoding toml: ",
        T.unpack (renderTOMLError tomlError)
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
  displayException (MkArbitraryE ex _) =
    mconcat
      [ "Exception:",
        displayException ex
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
  displayException = displayExceptions displayException

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
    Left ex ->
      throwCallStack @_ @ArbitraryE
        (MkArbitraryE ex)
    Right x -> pure x

-- | If 'True', uses 'displayTrace'; otherwise uses 'displayException'.
--
-- @since 0.1
displayTraceIf :: Exception e => Bool -> e -> Text
displayTraceIf False = T.pack . displayException
displayTraceIf True = displayTrace

-- | Like 'displayException', except we attempt to match the exceptions
-- specified in this module, and -- if found -- include the stack trace
-- in the display text. If we do fail to match, simply uses
-- 'displayException'.
--
-- @since 0.1
displayTrace :: Exception e => e -> Text
displayTrace e = case fromException someEx of
  Just ex@(MkExceptions _) ->
    builderToStrict $ displayExceptions traceSomeException ex
  Nothing -> builderToStrict $ traceSomeException someEx
  where
    someEx = toException e
    builderToStrict = TL.toStrict . TLB.toLazyText

traceSomeException :: SomeException -> Builder
traceSomeException e = case tryGetCallStack e of
  Left cs ->
    mconcat
      [ TLB.fromString $ displayException e,
        appendPrettyCs cs
      ]
  Right _ -> TLB.fromString $ displayException e

tryGetCallStack :: SomeException -> Either CallStack ()
tryGetCallStack someEx = do
  -- NOTE: Somewhat gross, but is simple and works. We take advantage
  -- of Left's early exit so the nesting does not reach Lovecraftian levels.
  go (view (_MkPathNotFoundE % _2)) someEx
  go (view (_MkRenameDuplicateE % _2)) someEx
  go (view (_MkReadIndexE % _3)) someEx
  go (view (_MkDuplicateIndexPathE % _3)) someEx
  go (view (_MkTrashPathNotFoundE % _3)) someEx
  go (view (_MkRestoreCollisionE % _3)) someEx
  go (view (_MkIndexSizeMismatchE % _4)) someEx
  go (view (_MkTomlDecodeE % _2)) someEx
  go (view (_MkArbitraryE % _2)) someEx
  where
    go ::
      forall e.
      Exception e =>
      (e -> CallStack) ->
      SomeException ->
      Either CallStack ()
    go getCs ex = case fromException @e ex of
      Just x -> Left $ getCs x
      Nothing -> Right ()

displayExceptions ::
  (IsString a, Monoid a) =>
  (SomeException -> a) ->
  Exceptions ->
  a
displayExceptions f (MkExceptions xs) =
  mconcat
    [ "Exceptions:",
      foldl' foldExs "" xs
    ]
  where
    foldExs acc ex = ("\n\n- " <> f ex) <> acc

appendPrettyCs :: CallStack -> Builder
appendPrettyCs cs =
  mconcat
    [ "\n\n",
      TLB.fromString (prettyCallStack cs)
    ]
