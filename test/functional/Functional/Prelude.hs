{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Prelude for functional test suite.
--
-- @since 0.1
module Functional.Prelude
  ( module X,

    -- * Types
    FuncEnv (..),

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

    -- * Misc
    getTestDir,
  )
where

import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Katip (Verbosity (V0))
import Katip qualified as K
import SafeRm.Data.Paths (PathI, PathIndex (TrashHome))
import SafeRm.Effects.Logger qualified as Logger
import SafeRm.Effects.Terminal (Terminal (putStr, putStrLn))
import SafeRm.Env (HasTrashHome)
import SafeRm.Exceptions (ExceptionI, ExceptionIndex (SomeExceptions))
import SafeRm.FileUtils as X
import SafeRm.Prelude as X
import SafeRm.Runner qualified as Runner
import SafeRm.Runner.Toml (TomlConfig)
import System.Exit (die)
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

-- NOTE: If we ever want to test logging, two options:
--
-- 1. Do not use Runner's runSafeRm. Instead, use its withEnv function with our
--    FunctionalIO, and ensure the latter implements Logger.
--
-- 2. Use golden tests for the logs. This will currently fail due to
--    timestamps, so abstract that to its own typeclass.

data FuncEnv = MkFuncEnv
  { trashHome :: !(PathI TrashHome),
    logEnv :: !LogEnv,
    logContexts :: !LogContexts,
    logNamespace :: !Namespace,
    terminalRef :: !(IORef Text),
    logsRef :: !(IORef Text)
  }

makeFieldLabelsNoPrefix ''FuncEnv

deriving anyclass instance HasTrashHome FuncEnv

-- | @since 0.1
newtype FunctionalIO a = MkFunctionalIO (ReaderT FuncEnv IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadReader FuncEnv,
      MonadUnliftIO
    )
    via (ReaderT FuncEnv IO)

instance Katip FunctionalIO where
  getLogEnv = asks (view #logEnv)
  localLogEnv f = local (over' #logEnv f)

instance KatipContext FunctionalIO where
  getKatipContext = asks (view #logContexts)
  localKatipContext f = local (over' #logContexts f)
  getKatipNamespace = asks (view #logNamespace)
  localKatipNamespace f = local (over' #logNamespace f)

-- | @since 0.1
instance Terminal FunctionalIO where
  putStr s = asks (view #terminalRef) >>= \ref -> modifyIORef' ref (<> T.pack s)
  putStrLn = putStr

-- | @since 0.1
runFunctionalIO :: FunctionalIO a -> FuncEnv -> IO a
runFunctionalIO (MkFunctionalIO rdr) = runReaderT rdr

-- | Runs safe-rm.
--
-- @since 0.1
runSafeRm :: [String] -> IO ()
runSafeRm = void . captureSafeRm

-- | Runs safe-rm and captures terminal output.
--
-- @since 0.1
captureSafeRm :: [String] -> IO [Text]
captureSafeRm argList = do
  terminalRef <- newIORef ""
  logsRef <- newIORef ""

  (toml, cmd) <- getConfig
  env <- mkFuncEnv toml logsRef terminalRef

  runFunctionalIO (Runner.runCmd cmd) env

  T.lines <$> readIORef terminalRef
  where
    argList' = "-c" : "none" : argList
    getConfig = SysEnv.withArgs argList' Runner.getConfiguration

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

mkFuncEnv :: TomlConfig -> IORef Text -> IORef Text -> IO FuncEnv
mkFuncEnv toml logsRef terminalRef = do
  initLogEnv <- K.initLogEnv "functional" "test"
  trashHome <- getTrashHome
  let scribe =
        Scribe
          { liPush = \item -> do
              let builder = Logger.consoleFormatter False V0 item
                  txt = TL.toStrict $ TLB.toLazyText builder

              modifyIORef' logsRef (<> txt),
            scribeFinalizer = pure (),
            -- TODO: capture everything and test
            scribePermitItem = const (pure True)
          }

  logEnv <-
    K.registerScribe
      "logger"
      scribe
      K.defaultScribeSettings
      initLogEnv

  pure $
    MkFuncEnv
      { trashHome = trashHome,
        logEnv,
        logContexts = mempty,
        logNamespace = "functional",
        terminalRef,
        logsRef
      }
  where
    getTrashHome = case toml ^. #trashHome of
      Nothing -> die "Setup error, no trash home on config"
      Just th -> pure th

replaceDir :: FilePath -> Text -> Text
replaceDir fp = T.replace (T.pack fp) "<dir>"

getTestDir :: IO FilePath
getTestDir = (</> "safe-rm/functional") <$> Dir.getTemporaryDirectory
