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

class HasTrashHome a where
  getTrashHome :: a -> PathI TrashHome

getTrashPaths :: HasTrashHome a => a -> (PathI TrashHome, PathI TrashIndex)
getTrashPaths x = (getTrashHome x, getTrashIndex x)

getTrashIndex :: HasTrashHome a => a -> PathI TrashIndex
getTrashIndex = liftPathI (</> ".index.csv") . getTrashHome

class HasVerbose a where
  getVerbose :: a -> Bool

data Env = MkEnv
  { trashHome :: !(PathI TrashHome),
    verbose :: !Bool
  }
  deriving stock
    ( Eq,
      Generic,
      Show
    )

instance HasTrashHome Env where
  getTrashHome = view #trashHome

instance HasVerbose Env where
  getVerbose = view #verbose
