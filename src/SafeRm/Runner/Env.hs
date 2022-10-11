{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides concrete Env type for running SafeRm.
--
-- @since 0.1
module SafeRm.Runner.Env
  ( Env (..),
  )
where

import SafeRm.Data.Paths (PathI, PathIndex (TrashHome))
import SafeRm.Env (HasTrashHome)
import SafeRm.Prelude

-- | Concrete environment type that can be used for running SafeRm
-- functions.
--
-- @since 0.1
data Env = MkEnv
  { -- | Trash home.
    --
    -- @since 0.1
    trashHome :: !(PathI TrashHome),
    -- | Katip log env.
    --
    -- @since 0.1
    logEnv :: !LogEnv,
    -- | Katip log context.
    --
    -- @since 0.1
    logContexts :: !LogContexts,
    -- | Katip log namespace.
    --
    -- @since 0.1
    logNamespace :: !Namespace
  }

makeFieldLabelsNoPrefix ''Env

-- | @since 0.1
deriving anyclass instance HasTrashHome Env
