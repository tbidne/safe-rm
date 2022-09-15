{-# LANGUAGE CPP #-}

-- | Main module.
--
-- @since 0.1
module Main (main) where

import Args
  ( Args (MkArgs, command),
    DelCommand
      ( DelCommandDelete,
        DelCommandEmpty,
        DelCommandList,
        DelCommandPermDelete,
        DelCommandRestore
      ),
    getArgs,
  )
import Control.Exception
  ( Exception (displayException, fromException, toException),
    SomeAsyncException (SomeAsyncException),
    SomeException,
    catch,
    throwIO,
  )
import Control.Monad ((<=<))
import Data.HashSet (HashSet)
import Data.HashSet qualified as Set
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
#if !MIN_VERSION_prettyprinter(1, 7, 1)
import Data.Text.Prettyprint.Doc (Pretty (pretty), layoutCompact)
import Data.Text.Prettyprint.Doc.Render.String (renderString)
#else
import Prettyprinter (Pretty (pretty), layoutCompact)
import Prettyprinter.Render.String (renderString)
#endif
import Del qualified

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = do
  MkArgs {command} <- getArgs
  let action = case command of
        DelCommandDelete mtrash paths -> Del.del mtrash paths
        DelCommandPermDelete mtrash paths ->
          Del.permDel mtrash (setToMap paths)
        DelCommandEmpty mtrash -> Del.empty mtrash
        DelCommandList mtrash -> listIndex mtrash
        DelCommandRestore mtrash paths ->
          Del.restore mtrash (setToMap paths)
  action `catchSync` handleEx
  where
    handleEx :: SomeException -> IO ()
    handleEx = putStrLn . displayException

listIndex :: Maybe FilePath -> IO ()
listIndex =
  putStrLn
    . renderString
    . layoutCompact
    . pretty
    <=< Del.getIndex

setToMap :: Hashable a => NonEmpty a -> HashSet a
setToMap = Set.fromList . NE.toList

catchSync :: Exception e => IO a -> (e -> IO a) -> IO a
catchSync io handler =
  io `catch` \ex ->
    case fromException (toException ex) of
      Just (SomeAsyncException _) -> throwIO ex
      Nothing -> handler ex
