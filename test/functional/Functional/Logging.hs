{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Tests for logging.
--
-- @since 0.1
module Functional.Logging
  ( tests,
  )
where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Time (LocalTime (LocalTime))
import Data.Time.LocalTime (midday)
import Functional.Prelude
import Numeric.Literal.Integer (FromInteger (afromInteger))
import SafeRm.Data.Paths (PathI, PathIndex (TrashHome))
import SafeRm.Effects.FileSystemReader (FileSystemReader (getFileSize))
import SafeRm.Effects.Logger
  ( LoggerContext
      ( getNamespace,
        localNamespace
      ),
    Namespace,
  )
import SafeRm.Effects.Logger qualified as Logger
import SafeRm.Effects.Terminal (Terminal (putStr, putStrLn), print)
import SafeRm.Effects.Timing (Timestamp (MkTimestamp), Timing (getSystemTime))
import SafeRm.Env (HasTrashHome)
import SafeRm.Runner qualified as Runner
import SafeRm.Runner.Env (Env)
import System.Environment qualified as SysEnv

data LoggerEnv = MkLoggerEnv
  { trashHome :: !(PathI TrashHome),
    logHandle :: !Handle,
    logFinalizer :: IO (),
    logLevel :: !LogLevel,
    logNamespace :: !Namespace
  }

makeFieldLabelsNoPrefix ''LoggerEnv

deriving anyclass instance HasTrashHome LoggerEnv

-- We need a new type here because we want real logging (so no FuncIO)
-- but also mocked timing, terminal, etc. (no SafeRmT).
newtype LoggerT a = MkLoggerT (ReaderT LoggerEnv IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadReader LoggerEnv,
      MonadUnliftIO
    )
    via (ReaderT LoggerEnv IO)

runLoggerT :: LoggerT a -> LoggerEnv -> IO a
runLoggerT (MkLoggerT r) = runReaderT r

instance MonadLogger LoggerT where
  monadLoggerLog loc _src lvl msg = do
    handle <- asks (view #logHandle)
    logLevel <- asks (view #logLevel)
    when (logLevel <= lvl) $ do
      formatted <- Logger.formatLog True loc lvl msg
      let bs = Logger.logStrToBs formatted
      print bs
      liftIO $ BS.hPut handle bs

instance LoggerContext LoggerT where
  getNamespace = asks (view #logNamespace)
  localNamespace = local . over' #logNamespace

instance FileSystemReader LoggerT where
  getFileSize = const (pure $ afromInteger 5)

instance Terminal LoggerT where
  putStr = const (pure ())
  putStrLn = putStr

instance Timing LoggerT where
  getSystemTime = pure $ MkTimestamp localTime
    where
      localTime = LocalTime (toEnum 59_000) midday

-- | @since 0.1
tests :: IO FilePath -> TestTree
tests args =
  testGroup
    "Logging"
    [ logging args
    ]

logging :: IO FilePath -> TestTree
logging args = goldenVsStringDiff desc diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "logging"
      trashDir = testDir </> ".trash"
      filesToDelete = (testDir </>) <$> ["f1", "f2", "f3"]
      delArgList =
        ["-t", trashDir, "--log-level", "debug"]
          <> ("d" : filesToDelete)

  -- setup
  clearDirectory testDir
  createFiles filesToDelete
  assertFilesExist filesToDelete

  bracket
    ( do
        (env, cmd) <- SysEnv.withArgs delArgList Runner.getEnv
        (,cmd) <$> transformEnv env
    )
    (\(loggerEnv, _) -> loggerEnv ^. #logFinalizer)
    ( \(loggerEnv, cmd) ->
        runLoggerT (Runner.runCmd cmd) loggerEnv
    )

  -- file assertions
  assertFilesExist
    ( (trashDir </>)
        <$> [".index.csv", "f1", "f2", "f3", ".log"]
    )
  assertFilesDoNotExist filesToDelete

  -- Test logging. In particular, test that startup does not fail when the
  -- log file does not already exist.
  BSL.fromStrict <$> BS.readFile (trashDir </> ".log")
  where
    desc = "Logging is successful"
    gpath = goldenPath </> "logs.golden"

goldenPath :: FilePath
goldenPath = "test/functional/Functional/Logging"

transformEnv :: Env -> IO LoggerEnv
transformEnv env = do
  (logHandle, logFinalizer, logLevel) <- case env ^. #logEnv % #logFile of
    Nothing -> throwString ""
    Just lf -> pure (lf ^. #handle, lf ^. #finalizer, lf ^. #logLevel)
  pure $
    MkLoggerEnv
      { trashHome = env ^. #trashHome,
        logHandle,
        logFinalizer,
        logLevel,
        logNamespace = "logging-tests"
      }
