{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import Box.Control
import Data.Text as Text

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> echo ""
    (t:_) -> echo (Text.pack t)

