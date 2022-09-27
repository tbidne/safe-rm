module Functional.Prelude
  ( module X,

    -- * Running Del
    runDel,
    captureDel,

    -- * File System Operations
    createFiles,
    createDirectories,
    clearDirectory,

    -- * Assertions
    assertFilesExist,
    assertFilesDoNotExist,
    assertDirectoriesExist,

    -- ** Text
    TextMatch (..),
    assertMatches,
  )
where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Del.Prelude as X
import Del.Runner (runDelHandler)
import System.Directory qualified as Dir
import System.Environment qualified as SysEnv
import Test.Tasty as X (TestTree, testGroup)
import Test.Tasty.HUnit as X
  ( assertBool,
    assertEqual,
    assertFailure,
    testCase,
    (@=?),
  )

-- | Runs del.
--
-- @since 0.1
runDel :: [String] -> IO ()
runDel argList =
  SysEnv.withArgs argList (runDelHandler (const (pure ())))

-- | Runs del and captures output.
--
-- @since 0.1
captureDel :: [String] -> IO [Text]
captureDel argList = do
  output <- newIORef ""
  let handler txt = modifyIORef' output (<> txt)
  SysEnv.withArgs argList (runDelHandler handler)
  T.lines <$> readIORef output

-- | Creates empty files at the specified paths.
--
-- @since 0.1
createFiles :: [FilePath] -> IO ()
createFiles paths = do
  for_ paths $ \p -> BS.writeFile p ""

-- | Creates empty files at the specified paths.
--
-- @since 0.1
createDirectories :: [FilePath] -> IO ()
createDirectories paths = do
  for_ paths $ \p -> Dir.createDirectoryIfMissing True p

-- | Clears a directory by deleting it if it exists and then recreating it.
--
-- @since 0.1
clearDirectory :: FilePath -> IO ()
clearDirectory path = do
  exists <- Dir.doesDirectoryExist path
  if exists
    then Dir.removePathForcibly path
    else pure ()
  createDirectoryIfMissing False path

-- | Asserts that files exist.
--
-- @since 0.1
assertFilesExist :: [FilePath] -> IO ()
assertFilesExist paths = do
  for_ paths $ \p -> do
    exists <- Dir.doesFileExist p
    assertBool ("Expected file to exist: " <> p) exists

-- | Asserts that files do not exist.
--
-- @since 0.1
assertFilesDoNotExist :: [FilePath] -> IO ()
assertFilesDoNotExist paths = do
  for_ paths $ \p -> do
    exists <- Dir.doesFileExist p
    assertBool ("Expected file not to exist: " <> p) (not exists)

-- | Asserts that directories exist.
--
-- @since 0.1
assertDirectoriesExist :: [FilePath] -> IO ()
assertDirectoriesExist paths = do
  for_ paths $ \p -> do
    exists <- Dir.doesDirectoryExist p
    assertBool ("Expected directory to exist: " <> p) exists

-- | Data type used for testing text matches.
--
-- @since 0.1
data TextMatch
  = Exact !Text
  | Prefix !Text
  | Infix !Text
  | Suffix !Text
  | Outfix !Text !Text
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

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

matches :: [TextMatch] -> [Text] -> Maybe String
matches [] [] = Nothing
matches s@(_ : _) [] =
  Just $ "Empty result but non-empty expectations: " <> show s
matches [] t@(_ : _) =
  Just $ "Empty expectations but non-empty result: " <> show t
matches (e : es) (t : ts) = isMatch (e :| es) (t :| ts)

isMatch :: NonEmpty TextMatch -> NonEmpty Text -> Maybe String
isMatch (s :| es) (r :| rs) =
  if isMatchHelper s (T.strip r)
    then matches es rs
    else
      Just $
        mconcat
          [ "Expected: ",
            showTextMatch s,
            "\nReceived: ",
            T.unpack r
          ]

isMatchHelper :: TextMatch -> Text -> Bool
isMatchHelper (Exact e) r = e == r
isMatchHelper (Prefix e) r = e `T.isPrefixOf` r
isMatchHelper (Infix e) r = e `T.isInfixOf` r
isMatchHelper (Suffix e) r = e `T.isSuffixOf` r
isMatchHelper (Outfix e1 e2) r = e1 `T.isPrefixOf` r && e2 `T.isSuffixOf` r

unlineMatches :: [TextMatch] -> String
unlineMatches [] = ""
unlineMatches (t : ts) = showTextMatch t <> "\n" <> unlineMatches ts

showTextMatch :: TextMatch -> String
showTextMatch (Exact e) = T.unpack e
showTextMatch (Prefix e) = T.unpack e <> wc
showTextMatch (Infix e) = wc <> T.unpack e <> wc
showTextMatch (Suffix e) = wc <> T.unpack e
showTextMatch (Outfix e1 e2) = T.unpack e1 <> wc <> T.unpack e2

wc :: String
wc = "**"
