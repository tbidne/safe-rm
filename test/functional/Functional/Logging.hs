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
import Data.Text.Encoding.Error qualified as TEncError
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLEnc
import Functional.Prelude
import SafeRm.Data.Paths (PathI, PathIndex (TrashHome))
import SafeRm.Effects.MonadLoggerContext
  ( LocStrategy (Stable),
    MonadLoggerContext
      ( getNamespace,
        localNamespace
      ),
    Namespace,
  )
import SafeRm.Effects.MonadLoggerContext qualified as Logger
import SafeRm.Effects.MonadTerminal
  ( print,
  )
import SafeRm.Env (HasTrashHome)
import SafeRm.Runner qualified as Runner
import SafeRm.Runner.Env (Env)
import System.Environment qualified as SysEnv

data LoggerEnv = MkLoggerEnv
  { trashHome :: !(PathI TrashHome),
    logHandle :: !Handle,
    logFinalizer :: IO (),
    logLevel :: !LogLevel,
    logNamespace :: !Namespace,
    terminalRef :: !(IORef Text)
  }

makeFieldLabelsNoPrefix ''LoggerEnv

deriving anyclass instance HasTrashHome LoggerEnv

instance MonadLogger (FuncIO LoggerEnv) where
  monadLoggerLog loc _src lvl msg = do
    handle <- asks (view #logHandle)
    logLevel <- asks (view #logLevel)
    when (logLevel <= lvl) $ do
      formatted <- Logger.formatLogLoc True (Stable loc) lvl msg
      let bs = Logger.logStrToBs formatted
      print bs
      liftIO $ BS.hPut handle bs

instance MonadLoggerContext (FuncIO LoggerEnv) where
  getNamespace = asks (view #logNamespace)
  localNamespace f = local (over' #logNamespace f)

-- | @since 0.1
tests :: IO FilePath -> TestTree
tests args =
  testGroup
    "Logging"
    [ logging args,
      excludesMetadata args
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

  runLogging delArgList

  -- file assertions
  assertFilesExist
    ( (trashDir </>)
        <$> [".index.csv", "f1", "f2", "f3", ".log"]
    )
  assertFilesDoNotExist filesToDelete

  -- Test logging. In particular, test that startup does not fail when the
  -- log file does not already exist.
  replaceDir testDir <$> BS.readFile (trashDir </> ".log")
  where
    desc = "Logging is successful"
    gpath = goldenPath </> "logs.golden"

excludesMetadata :: IO FilePath -> TestTree
excludesMetadata args = goldenVsStringDiff desc diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "logging"
      trashDir = testDir </> ".trash"
      filesToDelete = (testDir </>) <$> ["f1", "f2", "f3"]
      delArgList =
        ["-t", trashDir, "--log-level", "debug"]
          <> ("d" : filesToDelete)
      listArgList =
        ["-t", trashDir, "--log-level", "debug", "l"]
      metadataArgList =
        ["-t", trashDir, "--log-level", "debug", "m"]

  -- setup
  clearDirectory testDir
  createFiles filesToDelete
  assertFilesExist filesToDelete

  runLogging delArgList
  runLogging listArgList
  runLogging metadataArgList

  replaceDir testDir <$> BS.readFile (trashDir </> ".log")
  where
    desc = "Log file is excluded from metadata entries count"
    gpath = goldenPath </> "log-metadata-excluded.golden"

goldenPath :: FilePath
goldenPath = "test/functional/Functional/Logging"

transformEnv :: Env -> IO LoggerEnv
transformEnv env = do
  ref <- newIORef ""
  (logHandle, logFinalizer, logLevel) <- case env ^. #logEnv % #logFile of
    Nothing -> throwString ""
    Just lf -> pure (lf ^. #handle, lf ^. #finalizer, lf ^. #logLevel)
  pure $
    MkLoggerEnv
      { trashHome = env ^. #trashHome,
        logHandle,
        logFinalizer,
        logLevel,
        logNamespace = "logging-tests",
        terminalRef = ref
      }

-- HACK: See the note on Functional.Prelude.replaceDir. Note that we cannot
-- reuse that function directly since that operates on Text, whereas we have
-- to use ByteString here. Thus we implement the same idea here.
replaceDir :: FilePath -> ByteString -> BSL.ByteString
replaceDir fp =
  TLEnc.encodeUtf8
    . TL.replace (TL.pack fp) "<dir>"
    . TLEnc.decodeUtf8With TEncError.lenientDecode
    . BSL.fromStrict

runLogging :: [String] -> IO ()
runLogging argList = bracket mkEnv closeLog run
  where
    mkEnv = do
      (env, cmd) <- SysEnv.withArgs argList Runner.getEnv
      (,cmd) <$> transformEnv env
    closeLog = view (_1 % #logFinalizer)
    run (loggerEnv, cmd) = runFuncIO (Runner.runCmd cmd) loggerEnv
