-- | Prelude for benchmarks.
--
-- @since 0.1
module Benchmarks.Prelude
  ( module X,
    clearDirectory,
    header,
  )
where

import Data.ByteString.Char8 qualified as Char8
import Data.List qualified as L
import SafeRm.Data.PathData (headerNames)
import SafeRm.Prelude as X
import Test.Tasty.Bench as X
  ( Benchmark,
    bench,
    bgroup,
    defaultMain,
    nfIO,
  )
import UnliftIO.Directory qualified as Dir

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

-- | Csv header.
--
-- @since 0.1
header :: ByteString
header = Char8.pack $ L.intercalate "," headerNames
