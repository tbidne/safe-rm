-- | Provides the 'FileSystemReader' typeclass.
--
-- @since 0.1
module SafeRm.Effects.FileSystemReader
  ( FileSystemReader (..),
  )
where

import Control.Monad.Trans (MonadTrans (lift))
import Data.Bytes (Bytes (MkBytes), Size (B))
import SafeRm.Prelude
import UnliftIO.Directory qualified as Dir

-- | Represents a readable filesystem.
--
-- @since 0.1
class Monad m => FileSystemReader m where
  -- | Retrieves a file's size.
  --
  -- @since 0.1
  getFileSize :: FilePath -> m (Bytes B Natural)

-- | @since 0.1
instance FileSystemReader IO where
  getFileSize f = MkBytes . natToInt <$> Dir.getFileSize f
    where
      natToInt x
        | x < 0 =
            error $
              mconcat
                [ "[SafeRm.Effects.FileSystemReader]",
                  "getFileSize returned ",
                  show x,
                  " bytes for file: ",
                  f
                ]
        | otherwise = fromIntegral x

-- | @since 0.1
instance FileSystemReader m => FileSystemReader (ReaderT env m) where
  getFileSize = lift . getFileSize
