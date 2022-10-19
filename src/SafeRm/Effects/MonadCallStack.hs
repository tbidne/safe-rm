{-# LANGUAGE ImplicitParams #-}

-- | Provides the 'MonadCallStack' typeclass.
--
-- @since 0.1
module SafeRm.Effects.MonadCallStack
  ( MonadCallStack (..),
    throwCallStack,
  )
where

import Control.Monad.Trans (MonadTrans (lift))
import SafeRm.Prelude

-- | Typeclass for retrieving the 'CallStack'. The intention is that tests
-- can provide a pure implementation, whereas "real" code can use the usual
-- implicit params.
--
-- @since 0.1
type MonadCallStack :: (Type -> Type) -> Constraint
class Monad m => MonadCallStack m where
  -- | Retrieves the 'CallStack'.
  --
  -- @since 0.1
  getCallStack :: HasCallStack => m CallStack

-- | @since 0.1
instance MonadCallStack IO where
  getCallStack = pure ?callStack

-- | @since 0.1
instance MonadCallStack m => MonadCallStack (ReaderT e m) where
  getCallStack = lift getCallStack

-- | 'throwIO' with a 'callStack'.
--
-- @since 0.1
throwCallStack ::
  forall m e a.
  ( Exception e,
    HasCallStack,
    MonadCallStack m,
    MonadIO m
  ) =>
  (CallStack -> e) ->
  m a
throwCallStack f = getCallStack >>= throwIO . f
