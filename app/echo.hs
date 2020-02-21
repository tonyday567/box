{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude
import Control.Monad
import System.Process
import Control.Concurrent.Async
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text (Text)
import System.Environment

echoText :: Text -> IO ()
echoText p = do
  Text.putStrLn "first line"
  forever $ do
    t <- Text.getLine
    Text.putStrLn $ p <> t

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> echoText ""
    (t:_) -> echoText (Text.pack t)

{-
wtf!

λ: (hin, hout, _, _) <- runInteractiveCommand "/Users/tonyday/haskell/box/.stack-work/install/x86_64-osx/lts-14.13/8.6.5/bin/box-echo"
λ: hIsEOF hout
^CInterrupted.
λ: hIsEOF hout
False
λ: Text.hGetChunk hout
"echo start message\nwaiting ...\n"

-}
