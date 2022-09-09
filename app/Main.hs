-- | Main module.
--
-- @since 0.1
module Main (main) where

import Args
  ( Args (MkArgs, command),
    DelCommand (DelCommandDelete, DelCommandRestore),
    getArgs,
  )
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
  MkArgs {command} <- getArgs
  let action = case command of
        DelCommandDelete paths -> traverse_ Del.del paths
        DelCommandRestore paths -> traverse_ Del.restore paths

  action `catchSync` handleEx
  where
    handleEx :: SomeException -> IO ()
    handleEx = putStrLn . displayException

catchSync :: Exception e => IO a -> (e -> IO a) -> IO a
catchSync io handler =
  io `catch` \ex ->
    case fromException (toException ex) of
      Just (SomeAsyncException _) -> throwIO ex
      Nothing -> handler ex
