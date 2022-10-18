-- | Provides the 'MonadTerminal' typeclass.
--
-- @since 0.1
module SafeRm.Effects.MonadTerminal
  ( MonadTerminal (..),
    putText,
    putTextLn,
    print,
  )
where

import Control.Monad.Trans (MonadTrans (lift))
import Data.Text qualified as T
import SafeRm.Prelude
import System.IO qualified as IO

-- | Represents a terminal.
--
-- @since 0.1
type MonadTerminal :: (Type -> Type) -> Constraint
class Monad m => MonadTerminal m where
  -- | Simple print function without newline.
  --
  -- @since 0.1
  putStr :: HasCallStack => String -> m ()

  -- | Simple print function.
  --
  -- @since 0.1
  putStrLn :: HasCallStack => String -> m ()

  -- | Retrieves a user-supplied 'Char'.
  --
  -- @since 0.1
  getChar :: m Char

-- | @since 0.1
instance MonadTerminal IO where
  putStr = IO.putStr
  putStrLn = IO.putStrLn
  getChar = IO.getChar

instance MonadTerminal m => MonadTerminal (ReaderT e m) where
  putStr = lift . putStr
  putStrLn = lift . putStrLn
  getChar = lift getChar

-- | @since 0.1
putText :: (HasCallStack, MonadTerminal m) => Text -> m ()
putText = putStr . T.unpack

-- | @since 0.1
putTextLn :: (HasCallStack, MonadTerminal m) => Text -> m ()
putTextLn = putStrLn . T.unpack

-- | @since 0.1
print :: forall m a. (HasCallStack, Show a, MonadTerminal m) => a -> m ()
print = putStrLn . show
