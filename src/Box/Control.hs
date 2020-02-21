{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | An example of a Box for the command line.
module Box.Control
  ( ControlRequest (..),
    ControlResponse (..),
    Toggle(..),
    ControlBox,
    ControlBox_,
    ControlConfig (..),
    defaultControlConfig,
    consoleControlBox,
    consoleControlBox_,
    parseControlRequest,
    controlBox,
    testBoxManual,
    testBoxAuto,
    beep,
    timeOut,
    timedRequests,
  )
where

import Prelude
import Box
import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.Classy.IORef as C
import Control.Concurrent.Classy.STM.TVar as C
import Control.Lens hiding ((|>))
import Control.Monad
import Control.Monad.Conc.Class as C
import Control.Monad.STM.Class as C
import Control.Monad.Trans.Class
import qualified Data.Attoparsec.Text as A
import Data.Bool
import Data.Data
import Data.Functor
import Data.Maybe
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.IO as Text
import GHC.Generics
import qualified Streaming.Prelude as S

-- | request ADT
data ControlRequest
  = Check -- check for existence
  | Start -- start (if not yet started) idempotent
  | Stop -- cancel (without shutting down) idempotent
  | Reset -- stop and start (potentially cancelling a previous instance)
  | Quit -- stop & shutdown
  deriving (Show, Read, Eq, Data, Typeable, Generic)

-- | Parse command line requests
parseControlRequest :: A.Parser a -> A.Parser (Either ControlRequest a)
parseControlRequest pa =
    A.string "c" $> Left Check
    <|> A.string "s" $> Left Start
    <|> A.string "q" $> Left Stop
    <|> A.string "r" $> Left Reset
    <|> A.string "x" $> Left Quit
    <|> (Right <$> pa)

data Toggle = On | Off deriving (Show, Read, Eq, Generic)

-- | response ADT
data ControlResponse
  = ShuttingDown -- shutdown
  | Status (Toggle, Int) -- on/off and number restarts left
  | Info Text
  deriving (Show, Read, Eq, Generic)

-- | A 'Box' that only communicates via 'ControlRequest' and 'ControlResponse'
type ControlBox_ m = (MonadConc m) => Cont m (Box (STM m) ControlResponse ControlRequest)

-- | A 'Box' that communicates via 'ControlRequest' and 'ControlResponse' or an underlying typed-channel
type ControlBox a b m = (MonadConc m) => Cont m (Box (STM m) (Either ControlResponse a) (Either ControlRequest b))

-- |
data ControlConfig
  = ControlConfig
      { -- | maximum number of starts allowed
        starts :: Int,
        -- | whether to start automatically
        autoStart :: Bool,
        -- | whether to rerun with a delay if the action dies
        autoRestart :: Maybe Double,
        -- | logging debug Info
        debug :: Bool
      }
  deriving (Show, Eq, Ord)

-- | Default is one start, manual start and no autorestart.
defaultControlConfig :: ControlConfig
defaultControlConfig = ControlConfig 1 False Nothing False

-- | a command-line control box.
consoleControlBox :: ControlBox Text Text IO
consoleControlBox =
  Box
    <$> ( contramap (Text.pack . show)
            <$> (cStdout 1000 :: Cont IO (Committer (STM IO) Text))
        )
    <*> ( emap (pure . either (const Nothing) Just)
            <$> ( eParse (parseControlRequest A.takeText)
                    <$> eStdin 1000
                )
        )

-- | a command-line control box.
consoleControlBox_ :: ControlBox_ IO
consoleControlBox_ = bmap (pure . Just . Left) (pure . either Just (const Nothing)) <$>
  consoleControlBox

data ControlBoxState a = CBS {actionThread :: Maybe (Async ()), restartsLeft :: Int}

-- | an effect that can be started, stopped and restarted (a limited number of times)
controlBox ::
  ControlConfig ->
  IO a ->
  Box (STM IO) ControlResponse ControlRequest ->
  IO ()
controlBox (ControlConfig restarts' autostart autorestart debug') app (Box c e) = do
  info "controlBox"
  ref <- C.newIORef (CBS Nothing restarts')
  shut <- atomically $ newTVar False
  when autostart (info "autostart" >> start ref shut)
  info "race_"
  race_
    (go ref shut)
    (shutCheck shut)
  cancelThread ref
  info "controlBox end"
  where
    cancelThread r = do
      info "cancelThread"
      (CBS a n) <- readIORef r
      maybe (info "no thread found" >> pure ()) (\x -> cancel x >> info "thread cancelled") a
      writeIORef r (CBS Nothing n)
    shutCheck s = do
      info "shutCheck"
      atomically $ check =<< readTVar s
      info "shutCheck signal received"
    status r = do
      info "status"
      s <- C.readIORef r
      C.atomically
        ( void $
            commit
              c
              (Status (bool Off On (isJust (actionThread s)), restartsLeft s))
        )
    loopApp r s app' = do
      info "loopApp"
      _ <- app'
      info "post app'"
      checkRestarts r s
      info "maybe restarting"
      maybe (pure ()) (\t -> sleep t >> dec r >> loopApp r s app') autorestart
    dec r = do
      info "dec"
      cfg@(CBS _ n) <- readIORef r
      writeIORef r (cfg {restartsLeft = n-1})
    start r s = do
      info "start"
      (CBS a _) <- readIORef r
      when (isNothing a) $ do
        a' <-
          async
            ( do
                dec r
                loopApp r s app
                cfg <- readIORef r
                writeIORef r (cfg {actionThread = Nothing})
            )
        link a'
        cfg <- readIORef r
        writeIORef r (cfg {actionThread = Just a'})
    stop r s = do
      info "stop"
      cancelThread r
      checkRestarts r s
    info t = bool (pure ()) (void $ commit (liftC c) $ Info t) debug'
    shutdown = do
      info "shutDown"
      void $ commit (liftC c) ShuttingDown
    checkRestarts r s = do
      info "check restarts"
      (CBS _ n) <- C.readIORef r
      bool
        ( do
            atomically $ writeTVar s True
            shutdown
        )
        (pure ())
        (n > 0)
    go r s = do
      info "go"
      status r
      msg <- C.atomically $ emit e
      case msg of
        Nothing -> go r s
        Just msg' ->
          case msg' of
            Check ->
              go r s
            Start -> do
              start r s
              go r s
            Stop -> do
              stop r s
              go r s
            Quit -> stop r s >> shutdown
            Reset -> stop r s >> start r s >> go r s

-- | action for testing
beep :: Int -> Int -> Double -> IO ()
beep m x s = when (x <= m) (sleep s >> Text.putStrLn ("beep " <> Text.pack (show x)) >> beep m (x + 1) s)

-- | A box with a self-destruct timer.
timeOut :: Double -> ControlBox m a b
timeOut t =
  Box <$> mempty <*> ((lift (sleep t) >> S.yield (Left Quit)) & toEmit)

-- | a canned ControlRequest emitter with delays
timedRequests ::
  (MonadConc m) =>
  [(ControlRequest, Double)] ->
  Cont m (Emitter (STM m) ControlRequest)
timedRequests xs = toEmit $ foldr (>>) (pure ()) $ (\(a,t) -> lift (sleep t) >> S.yield a) <$> xs 

-- | manual testing
-- > testBoxManual (ControlConfig 1 True (Just 0.5) False) 2.3 (beep 3 1 0.5)
-- Status (On,0)
-- beep 1
-- beep 2
-- beep 3
-- Left ShutDown
testBoxManual :: ControlConfig -> Double -> IO () -> IO ()
testBoxManual cfg t effect =
  with (bmap (pure . Just . Left) (pure . either Just (const Nothing)) <$>
        consoleControlBox <> timeOut t) (controlBox cfg effect)

-- | auto testing
-- FIXME: Doesn't work with doctest
-- > testBoxAuto (ControlConfig 5 True (Just 0.2) False) 5 [(Check, 0.1), (Start,0.1), (Stop,1), (Start, 0.1), (Check, 0.1), (Reset,0.1)] (beep 2 1 1)
-- Left (Status (On,5))
-- Left (Status (On,4))
-- Left (Status (On,4))
-- beep 1
-- Left (Status (Off,4))
-- Left (Status (On,4))
-- Left (Status (On,3))
-- Left (Status (On,2))
-- beep 1
-- beep 2
-- beep 1
-- Left ShuttingDown
--
-- testBoxAuto (ControlConfig 1 True (Just 0.5) False) 3 [(Reset,1.1), (Quit, 1)] (beep 3 1 1)
-- Left (Status (On,1))
-- beep 1
-- Left ShuttingDown
-- Left (Status (On,-1))
--
testBoxAuto :: ControlConfig -> Double -> [(ControlRequest, Double)] -> IO () -> IO ()
testBoxAuto cfg t xs effect =
  with (bmap (pure . Just . Left) (pure . either Just (const Nothing)) <$>
        (consoleControlBox <>
         timeOut t <>
         (Box <$> mempty <*> (fmap Left <$> timedRequests xs)))) (controlBox cfg effect)
