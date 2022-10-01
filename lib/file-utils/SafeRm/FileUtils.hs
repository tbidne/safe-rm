-- | Provides utils for file system assertions.
--
-- @since 0.1
module SafeRm.FileUtils
  ( -- * File System Operations
    createFiles,
    createFileContents,
    createDirectories,
    clearDirectory,

    -- ** Text
    TextMatch (..),
    matches,
    unlineMatches,
  )
where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.Text qualified as T
import SafeRm.Prelude
import System.IO (putStrLn)
import UnliftIO.Directory qualified as Dir

-- | Creates empty files at the specified paths.
--
-- @since 0.1
createFiles :: [FilePath] -> IO ()
createFiles = createFileContents . fmap (,"")

-- | Creates files at the specified paths.
--
-- @since 0.1
createFileContents :: [(FilePath, ByteString)] -> IO ()
createFileContents paths = for_ paths $
  \(p, c) ->
    BS.writeFile p c
      `catchAny` \ex -> do
        putStrLn $
          mconcat
            [ "[SafeRm.FileUtils.createFileContents] Exception for file '",
              p,
              "' and contents '",
              Char8.unpack c,
              "': ",
              displayException ex
            ]
        throwIO ex

-- | Creates empty files at the specified paths.
--
-- @since 0.1
createDirectories :: [FilePath] -> IO ()
createDirectories paths =
  for_ paths $ \p -> Dir.createDirectoryIfMissing True p

-- | Clears a directory by deleting it if it exists and then recreating it.
--
-- @since 0.1
clearDirectory :: FilePath -> IO ()
clearDirectory path = do
  exists <- Dir.doesDirectoryExist path
  when exists $ Dir.removePathForcibly path
  createDirectoryIfMissing False path

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
