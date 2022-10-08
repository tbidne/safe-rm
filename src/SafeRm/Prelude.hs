-- | Custom prelude.
--
-- @since 0.1
module SafeRm.Prelude
  ( module X,

    -- * Text
    showt,

    -- * Container Operators
    Empty (..),
    Union (..),
    (⋃),
    Difference (..),
    (∆),
    Member (..),
    (∉),
    CMap (..),
  )
where

import Control.Applicative as X
  ( Alternative ((<|>)),
    Applicative (pure, (<*>)),
    (*>),
  )
import Control.DeepSeq as X (NFData)
import Control.Monad as X
  ( Monad ((>>=)),
    join,
    unless,
    void,
    when,
    (<=<),
    (>=>),
  )
import Control.Monad.Fail as X (MonadFail (fail))
import Control.Monad.IO.Class as X (MonadIO (liftIO))
import Control.Monad.Reader as X
  ( MonadReader (ask),
    ReaderT,
    asks,
    local,
    runReaderT,
  )
import Control.Monad.Trans as X (MonadTrans (lift))
import Data.Bool as X (Bool (False, True), not, otherwise, (&&), (||))
import Data.ByteString as X (ByteString)
import Data.Char as X (Char)
import Data.Either as X (Either (Left, Right))
import Data.Eq as X (Eq ((/=), (==)))
import Data.Foldable as X (Foldable (foldMap', foldl', foldr, length), for_, null)
import Data.Function as X (const, flip, id, ($), (.))
import Data.Functor as X (Functor (fmap), ($>), (<$>))
import Data.HashMap.Strict as X (HashMap)
import Data.HashMap.Strict qualified as Map
import Data.HashSet as X (HashSet)
import Data.HashSet qualified as Set
import Data.Hashable as X (Hashable (hashWithSalt))
import Data.Int as X (Int)
import Data.Kind as X (Constraint, Type)
import Data.List as X (zipWith)
import Data.List.NonEmpty as X (NonEmpty ((:|)))
import Data.Maybe as X (Maybe (Just, Nothing), fromMaybe, maybe)
import Data.Monoid as X (Monoid (mconcat, mempty))
import Data.Ord as X (Ord (compare, (<=), (>), (>=)), Ordering (EQ, GT, LT))
import Data.Proxy as X (Proxy (Proxy))
import Data.Semigroup as X (Semigroup ((<>)))
import Data.Sequence as X (Seq, (<|), (|>))
import Data.Sequence qualified as Seq
import Data.String as X (IsString (fromString), String)
import Data.Text as X (Text)
import Data.Text qualified as T
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
import GHC.Real as X (even, fromIntegral)
import GHC.Stack as X (HasCallStack)
import Optics.Core as X
  ( Iso',
    Lens',
    iso,
    over',
    review,
    view,
    (%),
    (^.),
    (^?),
    _1,
    _2,
  )
import Prettyprinter as X
  ( Pretty (pretty),
    layoutCompact,
    line,
    vsep,
    (<+>),
  )
import Prettyprinter.Render.Text as X (renderStrict)
import System.Exit as X (exitFailure)
import System.FilePath as X ((</>))
import System.IO as X
  ( BufferMode (NoBuffering),
    FilePath,
    IO,
  )
import Text.Show as X (Show (show))
import UnliftIO as X (MonadUnliftIO)
import UnliftIO.Directory as X (createDirectoryIfMissing)
import UnliftIO.Exception as X
  ( Exception (displayException, fromException, toException),
    SomeAsyncException (SomeAsyncException),
    SomeException,
    bracket,
    catch,
    catchAny,
    finally,
    handleAny,
    throwIO,
  )
import UnliftIO.IORef as X (IORef, modifyIORef', newIORef, readIORef)

showt :: Show a => a -> Text
showt = T.pack . show

-- | Types that have a notion of empty.
--
-- @since 0.1
class Empty α where
  -- | @since 0.1
  (∅) :: α

-- | @since 0.1
instance Empty (HashSet a) where
  (∅) = Set.empty

-- | @since 0.1
instance Empty (HashMap k v) where
  (∅) = Map.empty

-- | @since 0.1
instance Empty (Seq a) where
  (∅) = Seq.empty

-- | Types with a \'union\'.
--
-- @since 0.1
class Union α where
  -- | @since 0.1
  (∪) :: α -> α -> α

infixl 6 ∪

-- | @since 0.1
(⋃) :: (Empty α, Foldable ρ, Union α) => ρ α -> α
(⋃) = foldr (∪) (∅)

-- | @since 0.1
instance Hashable a => Union (HashSet a) where
  (∪) = Set.union

-- | @since 0.1
instance Hashable k => Union (HashMap k v) where
  (∪) = Map.union

-- | @since 0.1
instance Union (Seq a) where
  (∪) = (<>)

-- | Types with a \'difference\'.
--
-- @since 0.1
class Difference α where
  (∖) :: α -> α -> α

-- | @since 0.1
instance Hashable a => Difference (HashSet a) where
  (∖) = Set.difference

-- | @since 0.1
instance Hashable k => Difference (HashMap k v) where
  (∖) = Map.difference

-- | O(n^2).
--
-- @since 0.1
instance Eq a => Difference (Seq a) where
  -- NOTE: we could improve to O(n) or O(lg n) if we added a
  -- Hashable or Ord constraint, respectively.
  l ∖ r = foldr f (∅) l
    where
      f x acc
        | x ∈ r = acc |> x
        | otherwise = acc

infixl 6 ∖

-- | Symmetric difference.
--
-- @since 0.1
(∆) :: (Difference α, Union α) => α -> α -> α
x ∆ y = (x ∖ y) ∪ (y ∖ x)

infixl 6 ∆

-- | Types with a notion of \'member\'.
--
-- @since 0.1
class Member α where
  type MKey α
  (∈) :: MKey α -> α -> Bool

infix 4 ∈

-- | @since 0.1
instance Hashable a => Member (HashSet a) where
  type MKey (HashSet a) = a
  (∈) = Set.member

-- | @since 0.1
instance Hashable k => Member (HashMap k v) where
  type MKey (HashMap k v) = k
  (∈) = Map.member

instance Eq a => Member (Seq a) where
  type MKey (Seq a) = a
  (∈) x = Seq.null . Seq.filter (== x)

-- | Negation of '(∈)'.
--
-- @since 0.1
(∉) :: Member α => MKey α -> α -> Bool
(∉) x = not . (∈) x

infix 4 ∉

-- | Generalized 'fmap'.
--
-- @since 0.1
class CMap ρ where
  -- | Constraint on the image, if any.
  type CMapC ρ α :: Constraint

  φ :: CMapC ρ β => (α -> β) -> ρ α -> ρ β

-- | @since 0.1
instance CMap HashSet where
  type CMapC HashSet a = Hashable a
  φ = Set.map

-- | @since 0.1
instance CMap (HashMap k) where
  type CMapC (HashMap k) _ = ()
  φ = Map.map

instance CMap Seq where
  type CMapC Seq _ = ()
  φ = fmap
