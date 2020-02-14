module Main where

import Box
import Box.Control
import Control.Concurrent.Async
import Control.Lens hiding ((|>))
import Control.Monad.Conc.Class as C
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Streaming.Prelude as S
import System.IO
import System.Process

{-
The big problem here is using stdin for both comms to the box and as a pipe to the underlying processes stdin.

t1 & t2 output both look as if the input gets swallowed by one of the committers rather than gets propogated properly.


-}


-- | create a process, returning stdin, stdout and process handles
createP :: FilePath -> [Text] -> IO (Handle, Handle, ProcessHandle)
createP cmd args = do
  (Just inH, Just outH, Nothing, procH) <-
    createProcess
      (proc cmd (Text.unpack <$> args))
        { std_in = CreatePipe,
          std_out = CreatePipe
        }
  hSetBuffering inH LineBuffering
  pure (inH, outH, procH)

mkProcessBox :: FilePath -> [Text] -> IO (Cont IO (Box (STM IO) Text Text), ProcessHandle)
mkProcessBox cmd args = do
  (inH, outH, procH) <- createP cmd args
  pure
    ( Box
        <$> (Box.putLine inH & commitPlug)
        <*> (Box.getLine outH & emitPlug),
      procH
    )

transEcho :: Cont IO (Box (STM IO) Text Text) -> IO ()
transEcho outer = do
  (box', proch) <- mkProcessBox "/Users/tonyday/haskell/box/.stack-work/install/x86_64-osx/lts-14.13/8.6.5/bin/box-echo" [":"]
  let wireIn = Box <$> (committer <$> box') <*> ((emitter <$> outer) <> eStdin 10)
  let wireOut = Box <$> (committer <$> outer) <*> ((emitter <$> box') <> eStdin 10)
  let tq = Transducer $ \s -> s & S.takeWhile (/= "quit")
  _ <- race
    (etc () tq wireIn >> putStrLn "wireIn collapse")
    (etc () tq wireOut >> putStrLn "wireOut collapse")
  putStrLn "end of wire race"
  cleanupProcess (Nothing, Nothing, Nothing, proch)

testEcho :: ControlConfig -> Cont IO (Box (STM IO) ControlResponse ControlRequest) -> IO ()
testEcho cfg b =
  with b (controlBox cfg (transEcho echoBoxConsole'))

{-
λ: t1
Status (Off,0)
s
Status (On,0)
hello
s
s
s
"s"
swallowing
swallowing
Status (On,-1)
ShutDown
λ: box-echo: <stdin>: hGetLine: end of file
-}
t1 :: IO ()
t1 = testEcho defaultControlConfig consoleControlBox

controlTextBox ::
  Cont IO (Box (STM IO) Text Text)
controlTextBox =
  Box
    <$> ( contramap (Text.pack . show)
            <$> (cStdout 1000 :: Cont IO (Committer (STM IO) Text))
        )
    <*> eStdin 1000

echoBoxConsole ::
  Cont IO (Box (STM IO) (Either ControlResponse Text) (Either Text (Either ControlRequest Text)))
echoBoxConsole =
  Box
    <$> ( contramap (Text.pack . show)
            <$> (cStdout 1000 :: Cont IO (Committer (STM IO) Text))
        )
    <*> ( eParse parseControlRequest' <$> eStdin 1000
        )

ebInner :: Cont IO (Box (STM IO) Text Text)
ebInner =
  bmap
    (pure . Just . Right)
    (pure . either (const Nothing) (either (const Nothing) Just))
    <$> echoBoxConsole

ebControl :: Cont IO (Box (STM IO) ControlResponse ControlRequest)
ebControl =
  bmap
    (pure . Just . Left)
    (pure . either (const Nothing) (either Just (const Nothing)))
    <$> echoBoxConsole

echoBoxConsole' ::
  Cont IO (Box (STM IO) Text Text)
echoBoxConsole' =
  Box
    <$> ( contramap (Text.pack . show)
            <$> (cStdout 1000 :: Cont IO (Committer (STM IO) Text))
        )
    <*> ( keeps (_Right . _Right) <$> eParse parseControlRequest' <$> eStdin 1000
        )

{-
λ: t2
Left (Status (Off,4))
s
Left (Status (On,3))
hello
hello
hello
hello
Right "hello"
c
c
Left (Status (On,3))
Left (Status (On,3))
s
s
s
Right "s"
q
q
Left (Status (Off,3))
x
Left ShutDown
λ: box-echo: <stdin>: hGetLine: end of file
-}
t2 :: IO ()
t2 = with ebControl (controlBox (ControlConfig 4 False False 2) (transEcho ebInner))

eboth :: Cont IO (Emitter (STM IO) (Either Text ControlRequest))
eboth = emerge ((,) <$> (fmap Left . emitter <$> ebInner) <*> (fmap Right . emitter <$> ebControl))

main :: IO ()
main = pure ()
