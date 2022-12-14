-- | Property tests for the SafeRm API.
--
-- @since 0.1
module Integration.SafeRm
  ( tests,
  )
where

import Control.Monad.Reader (ReaderT (ReaderT))
import Data.Char qualified as Ch
import Data.HashMap.Strict qualified as HMap
import Data.HashSet qualified as HSet
import Data.List qualified as L
import Data.Sequence.NonEmpty qualified as NESeq
import Effects.MonadLoggerNamespace (MonadLoggerNamespace)
import Effects.MonadTime (MonadTime)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Integration.Prelude
import PathSize qualified
import SafeRm qualified
import SafeRm.Data.PathData (PathData)
import SafeRm.Data.Paths (PathI (MkPathI))
import SafeRm.Data.UniqueSeq (UniqueSeq, fromFoldable)
import SafeRm.Data.UniqueSeq qualified as USeq
import SafeRm.Exception (Exceptions (MkExceptions))
import SafeRm.Runner.Env
  ( Env (MkEnv),
    LogEnv (MkLogEnv),
    logEnv,
    trashHome,
  )
import SafeRm.Runner.SafeRmT (SafeRmT (..))

-- Custom type for running the tests. Fo now, the only reason we do not use
-- SafeRmT is to override getFileSize so that expected errors in tests
-- do not spam the console logs (i.e. retrieving the size for a bad path).
-- We could use this to later verify logs, if we wished.

-- | Type for running integration tests.
newtype IntIO a = MkIntIO (ReaderT Env IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadCallStack,
      MonadFileReader,
      MonadFileWriter,
      MonadHandleWriter,
      MonadPathReader,
      MonadPathWriter,
      MonadLogger,
      MonadLoggerNamespace,
      MonadReader Env,
      MonadTime,
      MonadTerminal,
      MonadUnliftIO
    )
    via (SafeRmT Env IO)

instance MonadPathSize IntIO where
  findLargestPaths _ _ =
    pure $
      PathSize.PathSizeSuccess $
        PathSize.MkSubPathData $
          NESeq.singleton $
            PathSize.MkPathData
              { PathSize.path = "",
                PathSize.size = 5,
                PathSize.numFiles = 10,
                PathSize.numDirectories = 20
              }

usingIntIO :: Env -> IntIO a -> IO a
usingIntIO env (MkIntIO rdr) = runReaderT rdr env

tests :: IO FilePath -> TestTree
tests testDir =
  testGroup
    "SafeRm"
    [ delete testDir,
      deleteSome testDir,
      deletePermanently testDir,
      deleteSomePermanently testDir,
      restore testDir,
      restoreSome testDir,
      emptyTrash testDir,
      metadata testDir
    ]

delete :: IO FilePath -> TestTree
delete mtestDir =
  testPropertyNamed "All paths are deleted" "delete" $ do
    property $ do
      testDir <- (</> "d1") <$> liftIO mtestDir
      ?? <- forAll genFileNameSet
      let ??Test = USeq.map (testDir </>) ??
          trashDir = testDir </> ".trash"
          ??Trash = USeq.map (trashDir </>) ??
      env <- liftIO $ mkEnv trashDir

      annotateShow ??Test

      -- create files and assert existence
      liftIO $ do
        clearDirectory testDir
        createFilesMap USeq.map ??Test
      assertFilesExist ??Test

      -- delete files
      liftIO $ usingIntIO env $ SafeRm.delete (USeq.map MkPathI ??Test)

      -- assert original files moved to trash
      annotate "Assert files exist"
      assertFilesExist ??Trash
      annotate "Assert files do not exist"
      assertFilesDoNotExist ??Test

      -- get index
      index <- liftIO $ view #unIndex <$> usingIntIO env SafeRm.getIndex
      annotateShow index

      let indexOrigPaths = HMap.foldl' toOrigPath HSet.empty index

      ??Test ^. #set === indexOrigPaths

deleteSome :: IO FilePath -> TestTree
deleteSome mtestDir =
  testPropertyNamed "Some paths are deleted, others error" "deleteSome" $ do
    property $ do
      testDir <- (</> "d2") <$> liftIO mtestDir
      (??, ??) <- forAll gen2FileNameSets
      let toTestDir = USeq.map (testDir </>)

          ??Test = toTestDir ??
          trashDir = testDir </> ".trash"
      env <- liftIO $ mkEnv trashDir

      annotateShow ??Test

      -- create files and assert existence
      liftIO $ do
        clearDirectory testDir
        createFilesMap USeq.map ??Test
      assertFilesExist ??Test

      -- delete files
      -- should succeed on ?? and fail on ??
      let toDelete = ??Test `USeq.union` toTestDir ??

      caughtEx <-
        liftIO $
          try @Exceptions $
            usingIntIO env (SafeRm.delete (USeq.map MkPathI toDelete))

      (MkExceptions exs) <-
        either
          pure
          (\_ -> annotate "Expected exceptions, received none" *> failure)
          caughtEx

      annotateShow exs

      -- should have received exactly 1 exception for each bad filename
      length exs === length ??

      -- assert original files moved to trash
      annotate "Assert files exist"
      assertFilesExist (USeq.map (trashDir </>) ??)
      annotate "Assert files do not exist"
      assertFilesDoNotExist (USeq.map (trashDir </>) ??)

      -- get index
      index <- liftIO $ view #unIndex <$> usingIntIO env SafeRm.getIndex
      annotateShow index

      let indexOrigPaths = HMap.foldl' toOrigPath HSet.empty index

      -- index should exactly match ??
      ??Test ^. #set === indexOrigPaths

deletePermanently :: IO FilePath -> TestTree
deletePermanently mtestDir =
  testPropertyNamed desc "deletePermanently" $ do
    property $ do
      testDir <- (</> "x1") <$> liftIO mtestDir
      ?? <- forAll genFileNameSet
      let trashDir = testDir </> ".trash"
          ??Test = USeq.map (testDir </>) ??
          ??Trash = USeq.map (trashDir </>) ??
      env <- liftIO $ mkEnv trashDir

      annotateShow ??Test

      -- create files and assert existence
      liftIO $ do
        clearDirectory testDir
        createFilesMap USeq.map ??Test
      assertFilesExist ??Test

      -- delete files
      liftIO $ usingIntIO env $ SafeRm.delete (USeq.map MkPathI ??Test)

      -- assert original files moved to trash
      annotate "Assert files exist"
      assertFilesExist ??Trash
      annotate "Assert files do not exist"
      assertFilesDoNotExist ??Test

      -- permanently delete files
      let toPermDelete = USeq.map MkPathI ??
      liftIO $ usingIntIO env $ SafeRm.deletePermanently True toPermDelete

      -- get index
      index <- liftIO $ view #unIndex <$> usingIntIO env SafeRm.getIndex
      annotateShow index

      HMap.empty === index
      assertFilesDoNotExist (??Test `USeq.union` ??Trash)
  where
    desc = "All trash entries are permanently deleted"

deleteSomePermanently :: IO FilePath -> TestTree
deleteSomePermanently mtestDir =
  testPropertyNamed desc "deleteSomePermanently" $ do
    property $ do
      testDir <- (</> "x2") <$> liftIO mtestDir
      (??, ??, ??) <- forAll gen3FileNameSets
      let toTestDir = USeq.map (testDir </>)
          toTrashDir = USeq.map (trashDir </>)

          toDelete = toTestDir (?? `USeq.union` ??)
          trashDir = testDir </> ".trash"
          trashSet = toTrashDir (?? `USeq.union` ??)
      env <- liftIO $ mkEnv trashDir

      annotateShow testDir
      annotateShow toDelete

      -- create files and assert existence
      liftIO $ do
        clearDirectory testDir
        createFilesMap USeq.map toDelete
      assertFilesExist toDelete

      -- delete files
      liftIO $ usingIntIO env $ SafeRm.delete (USeq.map MkPathI toDelete)

      -- assert original files moved to trash
      annotate "Assert files exist"
      assertFilesExist trashSet
      annotate "Assert files do not exist"
      assertFilesDoNotExist toDelete

      -- permanently delete files
      -- should succeed on ?? and fail on ??
      let toPermDelete = USeq.map MkPathI (?? `USeq.union` ??)
      annotateShow toPermDelete

      caughtEx <-
        liftIO $
          try @Exceptions $
            usingIntIO env (SafeRm.deletePermanently True toPermDelete)

      (MkExceptions exs) <-
        either
          pure
          (\_ -> annotate "Expected exceptions, received none" *> failure)
          caughtEx

      annotateShow exs

      -- get index
      index <- liftIO $ view #unIndex <$> usingIntIO env SafeRm.getIndex
      annotateShow index
      let indexOrigPaths = HMap.foldl' toOrigPath HSet.empty index

      -- ?? should still exist in the trash index
      toTestDir ?? ^. #set === indexOrigPaths

      annotate "Assert files do not exist"
      assertFilesDoNotExist (USeq.map (trashDir </>) (?? `USeq.union` ??))
      annotate "Assert files exist"
      assertFilesExist (USeq.map (trashDir </>) ??)
  where
    desc = "Some trash entries are permanently deleted, others error"

restore :: IO FilePath -> TestTree
restore mtestDir =
  testPropertyNamed "Restores all trash entries" "restore" $ do
    property $ do
      testDir <- (</> "r1") <$> liftIO mtestDir
      ?? <- forAll genFileNameSet
      let ??Test = USeq.map (testDir </>) ??
          trashDir = testDir </> ".trash"
          ??Trash = USeq.map (trashDir </>) ??
      env <- liftIO $ mkEnv trashDir

      annotateShow ??Test

      -- create files and assert existence
      liftIO $ do
        clearDirectory testDir
        createFilesMap USeq.map ??Test
      assertFilesExist ??Test

      -- delete files
      liftIO $ usingIntIO env $ SafeRm.delete (USeq.map MkPathI ??Test)

      -- assert original files moved to trash
      annotate "Assert files exist"
      assertFilesExist ??Trash
      annotate "Assert files do not exist"
      assertFilesDoNotExist ??Test

      -- restore files
      let toRestore = USeq.map MkPathI ??
      liftIO $ usingIntIO env $ SafeRm.restore toRestore

      -- get index
      index <- liftIO $ view #unIndex <$> usingIntIO env SafeRm.getIndex
      annotateShow index

      HMap.empty === index
      annotate "Assert files exist"
      assertFilesExist ??Test
      annotate "Assert files do not exist"
      assertFilesDoNotExist ??Trash

restoreSome :: IO FilePath -> TestTree
restoreSome mtestDir =
  testPropertyNamed desc "restoreSome" $ do
    property $ do
      testDir <- (</> "r2") <$> liftIO mtestDir
      (??, ??, ??) <- forAll gen3FileNameSets
      let toTestDir = USeq.map (testDir </>)
          toTrashDir = USeq.map (trashDir </>)

          toDelete = toTestDir (?? `USeq.union` ??)
          trashDir = testDir </> ".trash"
          trashSet = toTrashDir ??
      env <- liftIO $ mkEnv trashDir

      annotateShow testDir
      annotateShow toDelete

      -- create files and assert existence
      liftIO $ do
        clearDirectory testDir
        createFilesMap USeq.map toDelete
      assertFilesExist toDelete

      -- delete files
      liftIO $ usingIntIO env $ SafeRm.delete (USeq.map MkPathI toDelete)

      -- assert original files moved to trash
      annotate "Assert files exist"
      assertFilesExist trashSet
      annotate "Assert files do not exist"
      assertFilesDoNotExist toDelete

      -- restore
      -- should succeed on ?? and fail on ??
      let toRestore = USeq.map MkPathI (?? `USeq.union` ??)
      annotateShow toRestore

      caughtEx <-
        liftIO $
          try @Exceptions $
            usingIntIO env (SafeRm.restore toRestore)

      (MkExceptions exs) <-
        either
          pure
          (\_ -> annotate "Expected exceptions, received none" *> failure)
          caughtEx

      annotateShow exs

      -- get index
      index <- liftIO $ view #unIndex <$> usingIntIO env SafeRm.getIndex
      annotateShow index
      let indexOrigPaths = HMap.foldl' toOrigPath HSet.empty index

      -- ?? should still exist in the trash index
      toTestDir ?? ^. #set === indexOrigPaths

      annotate "Assert files exist"
      assertFilesExist (toTestDir ?? `USeq.union` toTrashDir ??)
      annotate "Assert files do not exist"
      assertFilesDoNotExist (toTrashDir ??)
  where
    desc = "Some trash entries are restored, others error"

emptyTrash :: IO FilePath -> TestTree
emptyTrash mtestDir =
  testPropertyNamed "Empties the trash" "empty" $ do
    property $ do
      testDir <- (</> "e1") <$> liftIO mtestDir
      ?? <- forAll genFileNameSet
      let aTest = USeq.map (testDir </>) ??
          trashDir = testDir </> ".trash"
          aTrash = USeq.map (trashDir </>) ??
      env <- liftIO $ mkEnv trashDir

      annotateShow testDir
      annotateShow aTest

      -- create files and assert existence
      liftIO $ do
        clearDirectory testDir
        createFilesMap USeq.map aTest
      assertFilesExist aTest

      -- delete files
      liftIO $ usingIntIO env $ SafeRm.delete (USeq.map MkPathI aTest)

      -- assert original files moved to trash
      annotate "Assert files exist"
      assertFilesExist aTrash
      annotate "Assert files do not exist"
      assertFilesDoNotExist aTest

      -- empty trash
      liftIO $ usingIntIO env $ SafeRm.emptyTrash True

      -- get index
      index <- liftIO $ view #unIndex <$> usingIntIO env SafeRm.getIndex
      annotateShow index

      HMap.empty === index
      annotate "Assert files do not exist"
      assertFilesDoNotExist (aTest `USeq.union` aTrash)

metadata :: IO FilePath -> TestTree
metadata mtestDir =
  testPropertyNamed "Retrieves metadata" "metadata" $ do
    property $ do
      testDir <- (</> "m1") <$> liftIO mtestDir
      ?? <- forAll genFileNameSet
      let aTest = USeq.map (testDir </>) ??
          trashDir = testDir </> ".trash"
          aTrash = USeq.map (trashDir </>) ??
      env <- liftIO $ mkEnv trashDir

      annotateShow testDir
      annotateShow aTest

      -- create files and assert existence
      liftIO $ do
        clearDirectory testDir
        createFilesMap USeq.map aTest
      assertFilesExist aTest

      -- delete files
      liftIO $ usingIntIO env $ SafeRm.delete (USeq.map MkPathI aTest)

      -- assert original files moved to trash
      annotate "Assert files exist"
      assertFilesExist aTrash
      annotate "Assert files do not exist"
      assertFilesDoNotExist aTest

      -- empty trash
      metadata' <- liftIO $ usingIntIO env SafeRm.getMetadata

      length ?? === natToInt (metadata' ^. #numEntries)
      length ?? === natToInt (metadata' ^. #numFiles)

natToInt :: HasCallStack => Natural -> Int
natToInt i
  | i <= intMax = fromIntegral i
  | otherwise = error $ "natToInt: Too large to convert to Int: " <> show i
  where
    intMax = fromIntegral (maxBound :: Int)

genFileNameSet :: Gen (UniqueSeq FilePath)
genFileNameSet = fromFoldable <$> Gen.list range genFileName
  where
    range = Range.linear 0 100

gen2FileNameSets :: Gen (UniqueSeq FilePath, UniqueSeq FilePath)
gen2FileNameSets = do
  ?? <- fromFoldable <$> Gen.list range genFileName
  ?? <- fromFoldable <$> Gen.list range (genFileNameNoDupes ??)
  pure (??, ??)
  where
    range = Range.linear 1 100

gen3FileNameSets :: Gen (UniqueSeq FilePath, UniqueSeq FilePath, UniqueSeq FilePath)
gen3FileNameSets = do
  ?? <- fromFoldable <$> Gen.list range genFileName
  ?? <- fromFoldable <$> Gen.list range (genFileNameNoDupes ??)
  ?? <- fromFoldable <$> Gen.list range (genFileNameNoDupes (?? `USeq.union` ??))
  pure (??, ??, ??)
  where
    range = Range.linear 1 100

genFileName :: Gen FilePath
genFileName = genFileNameNoDupes USeq.empty

genFileNameNoDupes :: UniqueSeq FilePath -> Gen FilePath
genFileNameNoDupes paths =
  Gen.filter
    (not . (`USeq.member` paths))
    (Gen.string range genChar)
  where
    range = Range.linear 1 20

genChar :: Gen Char
genChar = Gen.filterT (not . badChars) Gen.unicode
  where
    badChars c = Ch.isControl c || L.elem c ['/', '.']

toOrigPath :: HashSet FilePath -> PathData -> HashSet FilePath
toOrigPath acc pd = HSet.insert (pd ^. #originalPath % #unPathI) acc

mkEnv :: FilePath -> IO Env
mkEnv fp = do
  pure $
    MkEnv
      { trashHome = MkPathI fp,
        logEnv = MkLogEnv Nothing ""
      }
