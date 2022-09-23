-- | Main module.
--
-- @since 0.1
module Main (main) where

import Control.Exception
  ( Exception (..),
    SomeAsyncException (SomeAsyncException),
    SomeException,
    catch,
    throwIO,
  )
import Del.Runner (runDel)
import System.Exit (exitFailure)

main :: IO ()
main = runDel `catchSync` handleEx
  where
    handleEx :: SomeException -> IO ()
    handleEx ex = do
      putStrLn . displayException $ ex
      exitFailure

catchSync :: Exception e => IO a -> (e -> IO a) -> IO a
catchSync io handler =
  io `catch` \ex ->
    case fromException (toException ex) of
      Just (SomeAsyncException _) -> throwIO ex
      Nothing -> handler ex
