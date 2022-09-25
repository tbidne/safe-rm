-- | Provides types.
--
-- @since 0.1
module Del.Data.Index
  ( Index (..),
  )
where

import Data.HashMap.Strict qualified as Map
import Data.List qualified as L
import Del.Data.PathData (PathData, sortDefault)
import Del.Prelude

-- | Index that stores the trash data.
--
-- @since 0.1
newtype Index = MkIndex
  { unIndex :: HashMap FilePath PathData
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
      NFData
    )
  deriving
    ( -- | @since 0.1
      Semigroup,
      -- | @since 0.1
      Monoid
    )
    via (HashMap FilePath PathData)

-- | @since 0.1
instance Pretty Index where
  -- TODO: the order here is unstable. We should probably either try
  -- to preserve the original order, or allow some kind of sorting
  pretty =
    vsep
      . fmap pretty
      . L.sortBy sortDefault
      . Map.elems
      . view #unIndex
