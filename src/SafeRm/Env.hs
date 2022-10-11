{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides types and classes for running SafeRm with an environment.
--
-- @since 0.1
module SafeRm.Env
  ( HasTrashHome (..),
    getTrashPaths,
    getTrashIndex,
    Env (..),
  )
where

import Optics.Core (A_Getter, Is, LabelOptic')
import SafeRm.Data.Paths
  ( PathI,
    PathIndex (TrashHome, TrashIndex),
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
getTrashPaths :: HasTrashHome a => a -> (PathI TrashHome, PathI TrashIndex)
getTrashPaths x = (getTrashHome x, getTrashIndex x)

-- | Retrieves the trash index path.
--
-- @since 0.1
getTrashIndex :: HasTrashHome a => a -> PathI TrashIndex
getTrashIndex = liftPathI (</> ".index.csv") . getTrashHome

-- | Concrete environment type that can be used for running SafeRm
-- functions.
--
-- @since 0.1
data Env = MkEnv
  { trashHome :: !(PathI TrashHome),
    logEnv :: !LogEnv,
    logContexts :: !LogContexts,
    logNamespace :: !Namespace
  }

makeFieldLabelsNoPrefix ''Env

-- NOTE: manual because if we derive it anyclass then, because of TH
-- restrictions, the default signature actually can't see the instance.
-- It uses the 'Generic' optics implementation instead, if it exists.
instance HasTrashHome Env where
  getTrashHome = view #trashHome
