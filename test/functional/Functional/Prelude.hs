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
    captureSafeRmLogs,
    captureSafeRmExceptionLogs,

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
import Data.Time (LocalTime (LocalTime))
import Data.Time.LocalTime (midday)
import SafeRm.Data.Paths (PathI, PathIndex (TrashHome))
import SafeRm.Effects.Logger
  ( LoggerContext (getNamespace, localNamespace),
    Namespace,
  )
import SafeRm.Effects.Logger qualified as Logger
import SafeRm.Effects.Terminal (Terminal (putStr, putStrLn))
import SafeRm.Effects.Timing (Timestamp (MkTimestamp), Timing (getSystemTime))
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

data FuncEnv = MkFuncEnv
  { trashHome :: !(PathI TrashHome),
    terminalRef :: !(IORef Text),
    logsRef :: !(IORef Text),
    logNamespace :: !Namespace
  }

makeFieldLabelsNoPrefix ''FuncEnv

deriving anyclass instance HasTrashHome FuncEnv

-- | @since 0.1
newtype FuncIO a = MkFuncIO (ReaderT FuncEnv IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadReader FuncEnv,
      MonadUnliftIO
    )
    via (ReaderT FuncEnv IO)

-- | @since 0.1
instance Terminal FuncIO where
  putStr s = asks (view #terminalRef) >>= \ref -> modifyIORef' ref (<> T.pack s)
  putStrLn = putStr

instance Timing FuncIO where
  getSystemTime = pure $ MkTimestamp localTime
    where
      localTime = LocalTime (toEnum 59_000) midday

instance MonadLogger FuncIO where
  monadLoggerLog loc _src lvl msg = do
    formatted <- Logger.formatLog True loc lvl msg
    let txt = Logger.logStrToText formatted
    logsRef <- asks (view #logsRef)
    modifyIORef' logsRef (<> txt)

instance LoggerContext FuncIO where
  getNamespace = asks (view #logNamespace)
  localNamespace f = local (over' #logNamespace f)

-- | @since 0.1
runFuncIO :: FuncIO a -> FuncEnv -> IO a
runFuncIO (MkFuncIO rdr) = runReaderT rdr

-- | Runs safe-rm.
--
-- @since 0.1
runSafeRm :: [String] -> IO ()
runSafeRm = void . captureSafeRm

-- | Runs safe-rm and captures terminal output.
--
-- @since 0.1
captureSafeRm :: [String] -> IO [Text]
captureSafeRm = fmap (view _1) . captureSafeRmLogs

-- | Runs safe-rm and captures (terminal output, logs).
--
-- @since 0.1
captureSafeRmLogs :: [String] -> IO ([Text], [Text])
captureSafeRmLogs argList = do
  terminalRef <- newIORef ""
  logsRef <- newIORef ""

  (toml, cmd) <- getConfig
  env <- mkFuncEnv toml logsRef terminalRef

  runFuncIO (Runner.runCmd cmd) env

  testDir <- getTestDir
  terminal <- readIORef terminalRef
  logs <- replaceDir testDir <$> readIORef logsRef

  pure (T.lines terminal, T.lines logs)
  where
    argList' = "-c" : "none" : argList
    getConfig = SysEnv.withArgs argList' Runner.getConfiguration

-- | Runs safe-rm and captures a thrown exception and logs.
--
-- @since 0.1
captureSafeRmExceptionLogs :: Exception e => [String] -> IO (e, [Text])
captureSafeRmExceptionLogs argList = do
  terminalRef <- newIORef ""
  logsRef <- newIORef ""

  (toml, cmd) <- getConfig
  env <- mkFuncEnv toml logsRef terminalRef

  result <-
    runFuncIO
      ( (Runner.runCmd cmd $> Nothing)
          `catch` \e -> pure (Just e)
      )
      env

  case result of
    Nothing ->
      throwString
        "captureSafeRmExceptionLogs: Expected exception, received none"
    Just ex -> do
      testDir <- getTestDir
      logs <- replaceDir testDir <$> readIORef logsRef
      pure (ex, T.lines logs)
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
  trashHome <- getTrashHome
  pure $
    MkFuncEnv
      { trashHome = trashHome,
        terminalRef,
        logsRef,
        logNamespace = "functional"
      }
  where
    getTrashHome = case toml ^. #trashHome of
      Nothing -> die "Setup error, no trash home on config"
      Just th -> pure th

replaceDir :: FilePath -> Text -> Text
replaceDir fp = T.replace (T.pack fp) "<dir>"

getTestDir :: IO FilePath
getTestDir = (</> "safe-rm/functional") <$> Dir.getTemporaryDirectory
