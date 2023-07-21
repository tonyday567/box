{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

import Control.Applicative
import Control.Monad
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Semigroup
import Options.Applicative
import Perf
import Prelude
import Box

toListMTest :: Int -> IO [Int]
toListMTest n = toListM <$|> qListWith Unbounded [1..n]

pushListTest :: Int -> IO [Int]
pushListTest n = pushList <$|> (qListWith Unbounded [1..n])

data TestType = TestToListM | TestQList | TestDefault deriving (Eq, Show)

parseTestType :: Parser TestType
parseTestType =
  flag' TestToListM (long "toListM" <> help "test toListM speed")
    <|> flag' TestQList (long "qList" <> help "test qList speed")
    <|> pure TestDefault

data Options = Options
  { optionN :: Int,
    optionL :: Int,
    optionStatDType :: StatDType,
    optionTestType :: TestType,
    optionMeasureType :: MeasureType,
    optionGolden :: Golden,
    optionReportConfig :: ReportConfig,
    optionRawStats :: Bool
  }
  deriving (Eq, Show)

options :: Parser Options
options =
  Options
    <$> option auto (value 1000 <> long "runs" <> short 'n' <> help "number of tests to perform")
    <*> option auto (value 1000 <> long "length" <> short 'l' <> help "number of emits")
    <*> parseStatD
    <*> parseTestType
    <*> parseMeasure
    <*> parseGolden "golden"
    <*> parseReportConfig defaultReportConfig
    <*> switch (long "raw" <> short 'w' <> help "write raw statistics to file")

opts :: ParserInfo Options
opts =
  info
    (options <**> helper)
    (fullDesc <> progDesc "box benchmarking" <> header "speed performance")

main :: IO ()
main = do
  o <- execParser opts
  let !n = optionN o
  let !l = optionL o
  let t = optionTestType o
  let mt = optionMeasureType o
  let gold = goldenFromOptions [show mt, show n, show l] (optionGolden o)
  let w = optionRawStats o
  let raw = "other/" <> intercalate "-" [show mt, show t, show n] <> ".map"
  let cfg = optionReportConfig o

  case t of
    TestQList -> do
      error "nyi"
    TestToListM -> do
      error "nyi"
    TestDefault -> do
      m <- fmap (fmap (measureFinalStat mt)) $
        execPerfT (fmap (fmap average) $ measureDs mt n) $ do
          _ <- fam "sum fold" (pure $ sum [1..l])
          _ <- fam "toListM" (toListMTest l)
          _ <- fam "pushList" (pushListTest l)

          pure ()
      when w (writeFile raw (show m))
      report cfg gold (measureLabels mt) (Map.mapKeys (: []) (fmap (: []) m))
