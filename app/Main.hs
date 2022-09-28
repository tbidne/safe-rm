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
import SafeRm.Runner (runSafeRm)
import System.Exit (ExitCode (ExitSuccess), exitFailure)

main :: IO ()
main =
  runSafeRm
    `catchSync` doNothingOnSuccess
    `catchSync` handleEx
  where
    doNothingOnSuccess :: ExitCode -> IO ()
    doNothingOnSuccess ExitSuccess = pure ()
    doNothingOnSuccess ex = throwIO ex

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
