-- | Provides 'TestArgs'.
--
-- @since 0.1
module Functional.TestArgs
  ( TestArgs (..),
  )
where

import Functional.Prelude

-- | @since 0.1
newtype TestArgs = MkTestArgs
  { -- | @since 0.1
    tmpDir :: FilePath
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
