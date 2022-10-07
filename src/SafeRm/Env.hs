-- | Provides types and classes for running SafeRm with an environment.
--
-- @since 0.1
module SafeRm.Env
  ( HasTrashHome (..),
    getTrashPaths,
    getTrashIndex,
    HasVerbose (..),
    Env (..),
  )
where

import SafeRm.Data.Paths
import SafeRm.Prelude

-- | Class for retrieving the trash home.
--
-- @since 0.1
class HasTrashHome a where
  -- | Retrieves the trash home path.
  --
  -- @since 0.1
  getTrashHome :: a -> PathI TrashHome

-- | Retrieves all trash paths.
--
-- @since 0.1
getTrashPaths :: HasTrashHome a => a -> (PathI TrashHome, PathI TrashIndex)
getTrashPaths x = (getTrashHome x, getTrashIndex x)

-- | Retrieves the trash index path.
--
-- @since 0.1
getTrashIndex :: HasTrashHome a => a -> PathI TrashIndex
getTrashIndex = liftPathI (</> ".index.csv") . getTrashHome

-- | Class for retrieving verbosity.
--
-- @since 0.1
class HasVerbose a where
  -- | Retrieves the verbose flag.
  --
  -- @since 0.1
  getVerbose :: a -> Bool

-- | Concrete environment type that can be used for running SafeRm
-- functions.
--
-- @since 0.1
data Env = MkEnv
  { trashHome :: !(PathI TrashHome),
    verbose :: !Bool
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance HasTrashHome Env where
  getTrashHome = view #trashHome

-- | @since 0.1
instance HasVerbose Env where
  getVerbose = view #verbose
