{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Protolude
import Test.DocTest

main :: IO ()
main = doctest ["src/Etc.hs", "src/Etc/Committer.hs"]
