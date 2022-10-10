-- | Provides the 'Terminal' typeclass.
--
-- @since 0.1
module SafeRm.Effects.Terminal
  ( Terminal (..),
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
type Terminal :: (Type -> Type) -> Constraint
class Monad m => Terminal m where
  -- | Simple print function without newline.
  --
  -- @since 0.1
  putStr :: String -> m ()

  -- | Simple print function.
  --
  -- @since 0.1
  putStrLn :: String -> m ()

-- | @since 0.1
instance Terminal IO where
  putStr = IO.putStr
  putStrLn = IO.putStrLn

instance Terminal m => Terminal (ReaderT e m) where
  putStr = lift . putStr
  putStrLn = lift . putStrLn

-- | @since 0.1
putText :: Terminal m => Text -> m ()
putText = putStr . T.unpack

-- | @since 0.1
putTextLn :: Terminal m => Text -> m ()
putTextLn = putStrLn . T.unpack

-- | @since 0.1
print :: forall m a. (Show a, Terminal m) => a -> m ()
print = putStrLn . show
