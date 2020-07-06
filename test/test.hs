{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import NumHask.Prelude
import Test.DocTest

main :: IO ()
main = doctest
  [ "src/Box.hs",
    -- "src/Box/Committer.hs",
    "src/Box/IO.hs"
  ]
