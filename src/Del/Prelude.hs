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
    (*>),
  )
import Control.DeepSeq as X (NFData)
import Control.Exception as X
  ( Exception (displayException, fromException, toException),
    SomeAsyncException (SomeAsyncException),
    SomeException,
    bracket,
    catch,
    finally,
    throwIO,
  )
import Control.Monad as X (Monad ((>>=)), join, (<=<), (>=>))
import Control.Monad.Fail as X (MonadFail (fail))
import Data.Bool as X (Bool (False, True), not, otherwise, (&&))
import Data.ByteString as X (ByteString)
import Data.Either as X (Either (Left, Right))
import Data.Eq as X (Eq ((/=), (==)))
import Data.Foldable as X (Foldable (foldl', foldr, length), for_)
import Data.Function as X (const, flip, ($), (.))
import Data.Functor as X (Functor (fmap), ($>), (<$>))
import Data.HashMap.Strict as X (HashMap)
import Data.HashSet as X (HashSet)
import Data.Hashable as X (Hashable (hashWithSalt))
import Data.IORef as X (IORef, modifyIORef', newIORef, readIORef)
import Data.Int as X (Int)
import Data.Kind as X (Type)
import Data.List as X (zipWith)
import Data.List.NonEmpty as X (NonEmpty ((:|)))
import Data.Maybe as X (Maybe (Just, Nothing), maybe)
import Data.Monoid as X (Monoid (mconcat, mempty))
import Data.Ord as X (Ord (compare, (<=), (>), (>=)), Ordering (EQ, GT, LT))
import Data.Proxy as X (Proxy (Proxy))
import Data.Semigroup as X (Semigroup ((<>)))
import Data.String as X (IsString (fromString), String)
import Data.Text as X (Text)
import Data.Traversable as X (traverse)
import Data.Tuple as X (curry, uncurry)
import Data.Vector as X (Vector)
import Data.Word as X (Word16)
import GHC.Enum as X (Bounded (maxBound, minBound))
import GHC.Err as X (error, undefined)
import GHC.Float as X (Double)
import GHC.Generics as X (Generic)
import GHC.Integer as X (Integer)
import GHC.Natural as X (Natural)
import GHC.Num as X (Num ((+), (-)))
import GHC.Real as X (fromIntegral)
import GHC.Stack as X (HasCallStack)
import Optics.Core as X (Iso', iso, review, view, (%), (^.), _1, _2)
import Prettyprinter as X
  ( Pretty (pretty),
    layoutCompact,
    line,
    vsep,
    (<+>),
  )
import Prettyprinter.Render.Text as X (renderStrict)
import System.Directory as X (createDirectoryIfMissing)
import System.Exit as X (exitFailure)
import System.FilePath as X ((</>))
import System.IO as X
  ( BufferMode (NoBuffering),
    FilePath,
    IO,
    print,
    putStr,
    putStrLn,
  )
import Text.Show as X (Show (show))
