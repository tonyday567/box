{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Prelude
import Test.DocTest

main :: IO ()
main = doctest
  [ "src/Box.hs",
    -- "src/Box/Committer.hs",
    "src/Box/IO.hs"
    -- "app/websocket-tests.hs"
  ]
