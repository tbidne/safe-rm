{-# LANGUAGE OverloadedLists #-}

-- | Provides types.
--
-- @since 0.1
module Del.Data.PathData
  ( PathData (..),

    -- * Sorting
    sortDefault,
    sortCreated,
    sortName,
  )
where

import Data.Csv
  ( DefaultOrdered (headerOrder),
    FromNamedRecord,
    FromRecord,
    ToNamedRecord,
    ToRecord,
    (.:),
    (.=),
  )
import Data.Csv qualified as Csv
import Data.HashMap.Strict qualified as Map
import Del.Data.PathType (PathType (..))
import Del.Data.Timestamp (Timestamp (..))
import Del.Prelude
import GHC.Exts (IsList (Item))

-- TODO: probably somehow add the original index to the path data.

-- | Data for a path.
--
-- @since 0.1
data PathData = MkPathData
  { -- | The type of the path.
    --
    -- @since 0.1
    pathType :: !PathType,
    -- | The path to be used in the trash directory.
    --
    -- @since 0.1
    trashPath :: !FilePath,
    -- | The original path on the file system.
    --
    -- @since 0.1
    originalPath :: !FilePath,
    -- | Time this entry was created.
    --
    -- @since 0.1
    created :: !Timestamp
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
instance FromNamedRecord PathData where
  parseNamedRecord m =
    MkPathData
      <$> m .: "type"
      <*> m .: "trash"
      <*> m .: "original"
      <*> m .: "created"

-- | @since 0.1
instance ToNamedRecord PathData where
  toNamedRecord pd = Map.fromList $ zipWith (flip ($)) headerNames labelFn
    where
      labelFn =
        [ \x -> x .= (pd ^. #pathType),
          \x -> x .= (pd ^. #trashPath),
          \x -> x .= (pd ^. #originalPath),
          \x -> x .= (pd ^. #created)
        ]

-- | @since 0.1
instance DefaultOrdered PathData where
  headerOrder _ = headerNames

-- | @since 0.1
instance Pretty PathData where
  pretty pd = vsep strs <+> line
    where
      strs = zipWith (flip ($)) headerNames labelFn
      labelFn =
        [ \x -> x <> ":    " <+> pretty (pd ^. #pathType),
          \x -> x <> ":   " <+> pretty (pd ^. #trashPath),
          \x -> x <> ":" <+> pretty (pd ^. #originalPath),
          \x -> x <> ":" <+> pretty (pd ^. #created)
        ]

headerNames :: (IsList a, IsString (Item a)) => a
headerNames = ["type", "trash", "original", "created"]

-- | Sorts by the created date then the name.
--
-- @since 0.1
sortDefault :: PathData -> PathData -> Ordering
sortDefault x y = case sortCreated x y of
  EQ -> sortName x y
  other -> other

-- | Sorts by the created date.
--
-- @since 0.1
sortCreated :: PathData -> PathData -> Ordering
sortCreated = mapOrd (view #created)

-- | Sorts by the name.
--
-- @since 0.1
sortName :: PathData -> PathData -> Ordering
sortName = mapOrd (view #trashPath)

mapOrd :: Ord b => (a -> b) -> a -> a -> Ordering
mapOrd f x y = f x `compare` f y
