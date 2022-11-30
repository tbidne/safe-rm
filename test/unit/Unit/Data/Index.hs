-- | Unit tests for Data.Index
module Unit.Data.Index
  ( tests,
  )
where

import Data.ByteString.Lazy qualified as BSL
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLEnc
import Numeric.Literal.Integer (FromInteger (afromInteger))
import SafeRm.Data.Index (Index (MkIndex), Sort (Name, Size))
import SafeRm.Data.Index qualified as Index
import SafeRm.Data.PathData
  ( PathData (MkPathData),
    PathDataFormat (Multiline, Singleline),
  )
import SafeRm.Data.PathType (PathType (PathTypeDirectory, PathTypeFile))
import SafeRm.Data.Timestamp (Timestamp, fromText)
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Data.Index"
    [ formattingTests
    ]

formattingTests :: TestTree
formattingTests =
  testGroup
    "Formatting"
    [ format1,
      format2,
      format3,
      format4,
      format5,
      format6,
      format7,
      format8
    ]

format1 :: TestTree
format1 =
  goldenVsStringDiff desc diff gpath $
    toBS . Index.formatIndex Multiline Name False
      <$> mkIndex
  where
    desc = "Multiline, name, asc"
    gpath = goldenPath </> "multi-name-asc.golden"

format2 :: TestTree
format2 =
  goldenVsStringDiff desc diff gpath $
    toBS . Index.formatIndex Multiline Name True
      <$> mkIndex
  where
    desc = "Multiline, name, desc"
    gpath = goldenPath </> "multi-name-desc.golden"

format3 :: TestTree
format3 =
  goldenVsStringDiff desc diff gpath $
    toBS . Index.formatIndex Multiline Size False
      <$> mkIndex
  where
    desc = "Multiline, size, asc"
    gpath = goldenPath </> "multi-size-asc.golden"

format4 :: TestTree
format4 =
  goldenVsStringDiff desc diff gpath $
    toBS . Index.formatIndex Multiline Size True
      <$> mkIndex
  where
    desc = "Multiline, size, desc"
    gpath = goldenPath </> "multi-size-desc.golden"

format5 :: TestTree
format5 =
  goldenVsStringDiff desc diff gpath $
    toBS . Index.formatIndex (Singleline 10 22) Name False
      <$> mkIndex
  where
    desc = "Singeline, name, asc"
    gpath = goldenPath </> "single-name-asc.golden"

format6 :: TestTree
format6 =
  goldenVsStringDiff desc diff gpath $
    toBS . Index.formatIndex (Singleline 10 22) Name True
      <$> mkIndex
  where
    desc = "Singeline, name, desc"
    gpath = goldenPath </> "single-name-desc.golden"

format7 :: TestTree
format7 =
  goldenVsStringDiff desc diff gpath $
    toBS . Index.formatIndex (Singleline 10 22) Size False
      <$> mkIndex
  where
    desc = "Singeline, size, asc"
    gpath = goldenPath </> "single-size-asc.golden"

format8 :: TestTree
format8 =
  goldenVsStringDiff desc diff gpath $
    toBS . Index.formatIndex (Singleline 10 22) Size True
      <$> mkIndex
  where
    desc = "Singeline, size, desc"
    gpath = goldenPath </> "single-size-desc.golden"

mkIndex :: MonadFail f => f Index
mkIndex = do
  ts <- ts'
  pure $
    MkIndex $
      Index.fromList
        [ MkPathData PathTypeFile "foo" "/path/foo" (afromInteger 70) ts,
          MkPathData PathTypeFile "baz" "/path/bar/baz" (afromInteger 5_000) ts,
          MkPathData PathTypeDirectory "dir" "/some/really/really/long/dir" (afromInteger 20_230) ts,
          MkPathData PathTypeFile "f" "/foo/path/f" (afromInteger 13_070_000) ts,
          MkPathData PathTypeDirectory "d" "/d" largeFile ts,
          MkPathData PathTypeFile "z" "/z" (afromInteger 200_120) ts
        ]
  where
    -- 5,000 Y
    largeFile = afromInteger 5_000_000_000_000_000_000_000_000_000
    ts' :: MonadFail f => f Timestamp
    ts' = fromText "2020-05-31 12:00:00"

toBS :: Text -> BSL.ByteString
toBS = TLEnc.encodeUtf8 . TL.fromStrict

goldenPath :: FilePath
goldenPath = "test/unit/Unit/Data/Index"
