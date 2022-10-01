-- | Prelude for functional test suite.
--
-- @since 0.1
module Functional.Prelude
  ( module X,

    -- * Running SafeRm
    runSafeRm,
    captureSafeRm,

    -- * Assertions
    assertFilesExist,
    assertFilesDoNotExist,
    assertDirectoriesExist,
    assertDirectoriesDoNotExist,

    -- * Text assertion
    assertMatches,
    assertExceptionMatches,
  )
where

import Control.Monad.Reader (MonadReader (ask), ReaderT (ReaderT), runReaderT)
import Data.List qualified as L
import Data.Text qualified as T
import SafeRm.Effects.Terminal (Terminal (putStr, putStrLn))
import SafeRm.Exceptions (ExceptionI, ExceptionIndex (SomeExceptions))
import SafeRm.FileUtils as X
import SafeRm.Prelude as X hiding (IO)
import SafeRm.Runner qualified as Runner
import System.IO as X (IO)
import Test.Tasty as X (TestTree, testGroup)
import Test.Tasty.HUnit as X
  ( assertBool,
    assertEqual,
    assertFailure,
    testCase,
    (@=?),
  )
import UnliftIO.Directory qualified as Dir
import UnliftIO.Environment qualified as SysEnv

-- NOTE: The weird "hiding IO ... import IO" lines are so we don't trigger
-- -Wunused-packages wrt base (interferes with ghcid)

-- | @since 0.1
newtype FunctionalIO a = MkFunctionalIO (ReaderT (IORef Text) IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadReader (IORef Text),
      MonadUnliftIO
    )
    via (ReaderT (IORef Text) IO)

-- | @since 0.1
instance Terminal FunctionalIO where
  putStr s = ask >>= \ref -> modifyIORef' ref (<> T.pack s)
  putStrLn = putStr

-- | @since 0.1
runFunctionalIO :: FunctionalIO a -> IORef Text -> IO a
runFunctionalIO (MkFunctionalIO rdr) = runReaderT rdr

-- | Runs safe-rm.
--
-- @since 0.1
runSafeRm :: [String] -> IO ()
runSafeRm argList = do
  output <- newIORef ""
  runFunctionalIO funcIO output
  where
    funcIO = SysEnv.withArgs argList Runner.runSafeRm

-- | Runs safe-rm and captures output.
--
-- @since 0.1
captureSafeRm :: [String] -> IO [Text]
captureSafeRm argList = do
  output <- newIORef ""
  runFunctionalIO funcIO output
  T.lines <$> readIORef output
  where
    funcIO = SysEnv.withArgs argList Runner.runSafeRm

-- | Asserts that files exist.
--
-- @since 0.1
assertFilesExist :: [FilePath] -> IO ()
assertFilesExist paths =
  for_ paths $ \p -> do
    exists <- Dir.doesFileExist p
    assertBool ("Expected file to exist: " <> p) exists

-- | Asserts that files do not exist.
--
-- @since 0.1
assertFilesDoNotExist :: [FilePath] -> IO ()
assertFilesDoNotExist paths =
  for_ paths $ \p -> do
    exists <- Dir.doesFileExist p
    assertBool ("Expected file not to exist: " <> p) (not exists)

-- | Asserts that directories exist.
--
-- @since 0.1
assertDirectoriesExist :: [FilePath] -> IO ()
assertDirectoriesExist paths =
  for_ paths $ \p -> do
    exists <- Dir.doesDirectoryExist p
    assertBool ("Expected directory to exist: " <> p) exists

-- | Asserts that directories do not exist.
--
-- @since 0.1
assertDirectoriesDoNotExist :: [FilePath] -> IO ()
assertDirectoriesDoNotExist paths =
  for_ paths $ \p -> do
    exists <- Dir.doesDirectoryExist p
    assertBool ("Expected directory not to exist: " <> p) (not exists)

-- | Tests text for matches.
--
-- @since 0.1
assertMatches :: [TextMatch] -> [Text] -> IO ()
assertMatches expectations results = case matches expectations results of
  Nothing -> pure ()
  Just err ->
    assertFailure $
      mconcat
        [ err,
          "\n\n*** Full expectations ***\n\n",
          unlineMatches expectations,
          "\n*** Full results ***\n\n",
          T.unpack (T.unlines results)
        ]

-- | Tests text for exception matches. We sorted the exceptions textual
-- representation, so the listed exception expectations must match!
--
-- @since 0.1
assertExceptionMatches :: [TextMatch] -> ExceptionI SomeExceptions -> IO ()
assertExceptionMatches s exs = do
  case exTxt of
    [] -> pure ()
    (h : hs) -> do
      -- assert exception header
      "Encountered exception(s)" @=? h
      -- now do the rest, sorting the results since the order is
      -- non-deterministic
      assertMatches s (L.sort hs)
  where
    exTxt = T.lines . T.pack $ displayException exs
