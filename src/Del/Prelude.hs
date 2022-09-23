{-# LANGUAGE CPP #-}

-- | Custom prelude.
--
-- @since 0.1
module Del.Prelude
  ( module X,
  )
where

import Control.Applicative as X
  ( Alternative ((<|>)),
    Applicative (pure, (<*>)),
  )
import Control.DeepSeq as X (NFData)
import Control.Exception as X
  ( Exception (displayException, fromException, toException),
    SomeAsyncException (SomeAsyncException),
    SomeException,
    catch,
    finally,
    throwIO,
  )
import Control.Monad as X (Monad ((>>=)), join, (<=<), (>=>))
import Control.Monad.Fail as X (MonadFail (fail))
import Data.Bool as X (Bool (False, True), not, otherwise)
import Data.ByteString as X (ByteString)
import Data.Either as X (Either (Left, Right))
import Data.Eq as X (Eq ((==)))
import Data.Foldable as X (Foldable (foldl', foldr, length), for_)
import Data.Function as X (const, flip, ($), (.))
import Data.Functor as X (Functor (fmap), (<$>))
import Data.HashMap.Strict as X (HashMap)
import Data.HashSet as X (HashSet)
import Data.Hashable as X (Hashable)
import Data.IORef as X (IORef, modifyIORef', newIORef, readIORef)
import Data.Int as X (Int)
import Data.List as X (zipWith)
import Data.List.NonEmpty as X (NonEmpty ((:|)))
import Data.Maybe as X (Maybe (Just, Nothing), maybe)
import Data.Monoid as X (Monoid (mconcat, mempty))
import Data.Ord as X (Ord ((>)))
import Data.Semigroup as X (Semigroup ((<>)))
import Data.String as X (IsString (fromString), String)
import Data.Text as X (Text)
#if !MIN_VERSION_prettyprinter(1, 7, 1)
import Data.Text.Prettyprint.Doc as X
  ( Pretty (pretty),
    layoutCompact,
    line,
    vsep,
    (<+>),
  )
import Data.Text.Prettyprint.Doc.Render.Text as X (renderStrict)
#else
import Prettyprinter as X
  ( Pretty (pretty),
    layoutCompact,
    line,
    vsep,
    (<+>),
  )
import Prettyprinter.Render.Text as X (renderStrict)
#endif
import Data.Traversable as X (traverse)
import Data.Vector as X (Vector)
import Data.Word as X (Word16)
import GHC.Enum as X (Bounded (maxBound))
import GHC.Err as X (error)
import GHC.Float as X (Double)
import GHC.Generics as X (Generic)
import GHC.Integer as X (Integer)
import GHC.Natural as X (Natural)
import GHC.Num as X (Num ((+), (-)))
import GHC.Real as X (fromIntegral)
import GHC.Stack as X (HasCallStack)
import Optics.Core as X (view, (^.))
import System.FilePath as X ((</>))
import System.IO as X
  ( BufferMode (NoBuffering),
    FilePath,
    IO,
    putStr,
    putStrLn,
  )
import Text.Show as X (Show (show))
