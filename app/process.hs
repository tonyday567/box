{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Box
import Box.Control
import Control.Concurrent.Async
import Control.Concurrent.Classy.STM.TVar as C
import Control.Exception
import Control.Lens hiding ((|>))
import Control.Monad
import Control.Monad.Conc.Class as C
import Control.Monad.STM.Class
import qualified Data.Attoparsec.Text as A
import Data.Bool
import Data.Maybe
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.IO as Text
import System.IO
import qualified System.Process as P
import System.Process.Typed
import Prelude

boxEcho :: FilePath
boxEcho = "/Users/tonyday/haskell/box/.stack-work/install/x86_64-osx/lts-14.13/8.6.5/bin/box-echo"

cannedCat :: ProcessConfig Handle Handle ()
cannedCat =
  setStdin createPipe
    $ setStdout createPipe
    $ setStderr closed
      "cat"

-- λ: c <- cat'
-- λ: hPutStrLn (getStdin e) "Hello!"
-- λ: hGetLine (getStdout c)
-- "Hello!"
cat' :: IO (Process Handle Handle ())
cat' = do
  p' <- startProcess cannedCat
  hSetBuffering (getStdout p') NoBuffering
  hSetBuffering (getStdin p') NoBuffering
  pure p'

-- λ: tCat
-- "Hello!"
tCat :: IO ()
tCat =
    withProcessTerm cannedCat $ \p -> do
        hPutStrLn (getStdin p) "Hello!"
        hFlush (getStdin p)
        hGetLine (getStdout p) >>= print

cannedEcho :: ProcessConfig Handle Handle ()
cannedEcho =
  setStdin createPipe
    $ setStdout createPipe
    -- $ setStderr closed
    $ proc boxEcho []

-- λ: e <- echo'
-- λ: hPutStrLn (getStdin e) "Hello!"
-- λ: hGetLine (getStdout e)
-- ^CInterrupted.
-- λ: hGetLine (getStdout e)
-- "first line"
-- λ: hGetLine (getStdout e)
-- "Hello!"
echo' :: IO (Process Handle Handle ())
echo' = do
  p' <- startProcess cannedEcho
  hSetBuffering (getStdout p') NoBuffering
  hSetBuffering (getStdin p') NoBuffering
  pure p'

-- λ: tEcho
-- ^CInterrupted.
tEcho :: IO ()
tEcho =
    withProcessTerm cannedEcho $ \p -> do
        hPutStrLn (getStdin p) "Hello!"
        hFlush (getStdin p)
        hGetLine (getStdout p) >>= print

{-

old-school process testing

-}
-- | create a process, returning stdin, stdout and process handles
createP :: FilePath -> [Text] -> IO (Handle, Handle, P.ProcessHandle)
createP cmd args = do
  (Just inH, Just outH, Nothing, procH) <-
    P.createProcess
      (P.proc cmd (Text.unpack <$> args))
        { P.std_in = P.CreatePipe,
          P.std_out = P.CreatePipe
        }
  hSetBuffering inH LineBuffering
  pure (inH, outH, procH)

testCreatePEcho :: IO ()
testCreatePEcho = do
  (i, o, p) <- createP "/Users/tonyday/haskell/box/.stack-work/install/x86_64-osx/lts-14.13/8.6.5/bin/box-echo" []
  putStrLn "createP created"
  hPutStrLn i "test input"
  hFlush i
  t <- hGetLine o
  putStrLn t
  P.cleanupProcess (Nothing, Nothing, Nothing, p)

testCreatePCat :: IO ()
testCreatePCat = do
  (i, o, p) <- createP "cat" []
  putStrLn "createP created"
  hPutStrLn i "test input"
  hFlush i
  t <- hGetLine o
  putStrLn t
  P.cleanupProcess (Nothing, Nothing, Nothing, p)



-- control box process
data CBP = CBP {listenThread :: Maybe (Async ()), process :: Maybe (Process Handle Handle ()), restarts :: Int}

-- | an effect that can be started, stopped and restarted (a limited number of times)
controlBoxProcess ::
  ControlConfig ->
  ProcessConfig Handle Handle () ->
  Box (STM IO) (Either ControlResponse Text) (Either ControlRequest Text) ->
  IO ()
controlBoxProcess (ControlConfig restarts' autostart _ debug') pc (Box c e) = do
  info "controlBoxProcess"
  ref <- C.newIORef (CBP Nothing Nothing restarts')
  shut <- atomically $ C.newTVar False
  when autostart (info "autostart" >> start ref shut)
  info "race_"
  race_
    (go ref shut)
    (shutCheck shut)
  cancelThread ref
  info "controlBoxProcess end"
  where
    cancelThread r = do
      info "cancelThread"
      a <- readIORef r
      maybe (info "no listener on cancelThread") (\x -> cancel x >> info "listener cancelled") (listenThread a)
      maybe (info "no process on cancelThread") (\x -> stopProcess x >> info "process cancelled") (process a)
      writeIORef r (CBP Nothing Nothing (restarts a))
    shutCheck s = do
      info "shutCheck"
      atomically $ check =<< readTVar s
      info "shutCheck signal received"
    status r = do
      info "status"
      a <- C.readIORef r
      C.atomically
        ( void $
            commit
              c
              (Left $ Status (bool Off On (isJust (process a)), restarts a))
        )
    loopApp r _ = do
      info "loopApp"
      p' <- startProcess pc
      a <- readIORef r
      when (isJust (process a)) (info "eeek, a process ref has been overwritten")
      when (isJust (listenThread a)) (info "eeek, a listener ref has been overwritten")
      info "process is up"
      wo <- async (lloop0 (getStdout p') `finally` info "wo finished")
      writeIORef r (CBP (Just wo) (Just p') (restarts a))
      info "listener is up"
      link wo
    lloop0 o = do
      b <- hIsEOF o
      when (not b) (checkOutH o >> lloop0 o)
    checkOutH o = do
      info "waiting for process output"
      t <- Text.hGetLine o
      info ("received: " <> t)
      C.atomically $ void $ commit (contramap Right c) t
    dec r = do
      info "dec"
      a <- readIORef r
      writeIORef r (a {restarts = restarts a - 1})
    start r s = do
      info "start"
      a <- readIORef r
      when (isNothing (process a)) $ do
        dec r
        loopApp r s
    stop r s = do
      info "stop"
      cancelThread r
      checkRestarts r s
    info t = bool (pure ()) (void $ commit (liftC c) $ Left (Info t)) debug'
    shutdown = do
      info "shutDown"
      void $ commit (liftC c) (Left ShuttingDown)
    checkRestarts r s = do
      info "check restarts"
      n <- restarts <$> C.readIORef r
      bool
        ( do
            atomically $ writeTVar s True
            shutdown
        )
        (pure ())
        (n > 0)
    writeIn r t = do
      info ("writeIn: " <> t)
      p <- process <$> C.readIORef r
      maybe
        (info "no stdin available")
        (\i -> hPutStrLn (getStdin i) (Text.unpack t) >> hFlush (getStdin i))
        p
    go r s = do
      info "go"
      status r
      msg <- C.atomically $ emit e
      case msg of
        Nothing -> go r s
        Just msg' ->
          case msg' of
            Left Check ->
              go r s
            Left Start -> do
              start r s
              go r s
            Left Stop -> do
              stop r s
              go r s
            Left Quit -> stop r s >> shutdown
            Left Reset -> stop r s >> start r s >> go r s
            Right t -> writeIn r t >> go r s

controlConsole ::
  Cont IO (Box (STM IO) (Either ControlResponse Text) (Either ControlRequest Text))
controlConsole =
  Box
    <$> ( contramap (Text.pack . show)
            <$> (cStdout 1000 :: Cont IO (Committer (STM IO) Text))
        )
    <*> ( fmap (either (Right . ("parse error: " <>)) id)
            . eParse (parseControlRequest A.takeText) <$> eStdin 1000
        )

testCatControl :: ControlConfig -> IO ()
testCatControl cfg = with controlConsole (controlBoxProcess cfg cannedCat)

main :: IO ()
main = testCatControl defaultControlConfig
