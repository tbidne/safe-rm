module Benchmarks.Prelude
  ( module X,
    clearDirectory,
    header,
  )
where

import Data.ByteString.Char8 qualified as Char8
import Data.List qualified as L
import Del.Data.PathData (headerNames)
import Del.Prelude as X
import System.Directory qualified as Dir
import Test.Tasty.Bench as X
  ( Benchmark,
    bench,
    bgroup,
    defaultMain,
    nfIO,
  )

clearDirectory :: FilePath -> IO ()
clearDirectory path = do
  exists <- Dir.doesDirectoryExist path
  if exists
    then Dir.removePathForcibly path
    else pure ()
  createDirectoryIfMissing False path

header :: ByteString
header = Char8.pack $ L.intercalate "," headerNames
