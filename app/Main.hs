-- | Main module.
--
-- @since 0.1
module Main (main) where

import Args (Args (MkArgs, paths), getArgs)
import Control.Exception
  ( Exception (displayException, fromException, toException),
    SomeAsyncException (SomeAsyncException),
    SomeException,
    catch,
    throwIO,
  )
import Data.Foldable (traverse_)
import Del qualified

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = do
  MkArgs {paths} <- getArgs
  traverse_ Del.del paths `catchSync` handleEx
  where
    handleEx :: SomeException -> IO ()
    handleEx = putStrLn . displayException

catchSync :: Exception e => IO a -> (e -> IO a) -> IO a
catchSync io handler =
  io `catch` \ex ->
    case fromException (toException ex) of
      Just (SomeAsyncException _) -> throwIO ex
      Nothing -> handler ex
