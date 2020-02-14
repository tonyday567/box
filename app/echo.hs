{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import Data.Text as Text
import Data.Text.IO as Text
import Control.Monad

-- experiments with a process
echo :: Text -> IO ()
echo p = forever $ do
  t <- Text.getLine
  Text.putStrLn $ p <> t

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> echo ""
    (t:_) -> echo (Text.pack t)

