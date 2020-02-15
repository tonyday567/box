{-# OPTIONS_GHC -Wall #-}

module Main where

import Prelude
import Test.DocTest

main :: IO ()
main = doctest
  [ "src/Box.hs"
    -- "src/Box/Control.hs"
  ]
