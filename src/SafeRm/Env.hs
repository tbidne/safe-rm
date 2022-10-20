-- | Provides classes for running SafeRm with an environment.
--
-- @since 0.1
module SafeRm.Env
  ( HasTrashHome (..),
    getTrashPaths,
    getTrashIndex,
    getTrashLog,
  )
where

import SafeRm.Data.Paths
  ( PathI,
    PathIndex (TrashHome, TrashIndex, TrashLog),
    liftPathI,
  )
import SafeRm.Prelude

-- | Class for retrieving the trash home.
--
-- @since 0.1
class HasTrashHome a where
  -- | Retrieves the trash home path.
  --
  -- @since 0.1
  getTrashHome :: a -> PathI TrashHome
  default getTrashHome ::
    ( Is k A_Getter,
      LabelOptic' "trashHome" k a (PathI TrashHome)
    ) =>
    a ->
    PathI TrashHome
  getTrashHome = view #trashHome

-- | Retrieves all trash paths.
--
-- @since 0.1
getTrashPaths ::
  HasTrashHome a =>
  a ->
  (PathI TrashHome, PathI TrashIndex, PathI TrashLog)
getTrashPaths x = (getTrashHome x, getTrashIndex x, getTrashLog x)

-- | Retrieves the trash index path.
--
-- @since 0.1
getTrashIndex :: HasTrashHome a => a -> PathI TrashIndex
getTrashIndex = liftPathI (</> ".index.csv") . getTrashHome

-- | Retrieves the trash log path.
--
-- @since 0.1
getTrashLog :: HasTrashHome a => a -> PathI TrashLog
getTrashLog = liftPathI (</> ".log") . getTrashHome
