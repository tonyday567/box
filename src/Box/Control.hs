{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Box.Control
  ( ControlComm(..)
  , ControlBox
  , ControlConfig(..)
  , consoleControlBox
  , parseControlComms
  , controlBox
  , runControlBox
  , testBox
  , timeOut
  ) where

import Box
import Control.Applicative
import Control.Category
import Control.Concurrent.Async
import Control.Lens hiding ((|>))
import Control.Monad
import Data.Data
import Data.Default
-- import Data.IORef
import Flow
import GHC.Generics
import Protolude hiding ((.), STM)
import Text.Read (readMaybe)
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as Text
import qualified Streaming.Prelude as S
import Control.Monad.Conc.Class as C

data ControlComm
  = Ready -- ready for comms
  | Check -- check for existence
  | Died -- died (of its own accord)
  | Stop -- stop (without shutting down)
  | Kill -- stop and quit (& cancel thread)
  | ShutDown -- successfully Killed
  | Start -- start (if not yet started)
  | Reset -- stop and start (potentially cancelling a previous instance)
  | On Bool -- are we live?
  | Log Text
  deriving (Show, Read, Eq, Data, Typeable, Generic)

type ControlBox m = (MonadConc m) => Cont m (Box (STM m) ControlComm ControlComm)

data ControlConfig
  = KeepAlive Double
  | AllowDeath
  deriving (Show, Eq)

instance Default ControlConfig where
  def = AllowDeath

consoleControlBox :: ControlBox IO
consoleControlBox =
  Box <$>
  (contramap show <$>
   (cStdout 1000 :: Cont IO (Committer (STM IO) Text))) <*>
  (emap (pure . either (const Nothing) Just) <$>
   (eParse parseControlComms <$>
    eStdin 1000))

parseControlComms :: A.Parser ControlComm
parseControlComms =
  A.string "q" *> return Stop <|> A.string "s" *> return Start <|>
  A.string "x" *> return Kill <|> do
    res <- readMaybe . Text.unpack <$> A.takeText
    case res of
      Nothing -> mzero
      Just a -> return a

-- | an effect that can be started and stopped
-- committer is an existence test
-- controlBox :: (MonadConc m) => ControlConfig -> m () -> ControlBox m
controlBox
  :: ControlConfig
     -> IO a -> Box (STM IO) ControlComm ControlComm -> IO Bool
controlBox cfg app (Box c e) = do
  ref' <- C.newIORef Nothing
  go ref'
  where
    go ref = do
      msg <- C.atomically $ emit e
      case msg of
        Nothing -> go ref
        Just msg' ->
          case msg' of
            Check -> do
              a <- C.readIORef ref
              _ <-
                C.atomically $ commit c $ On (bool True False (isNothing a))
              go ref
            Start -> do
              a <- C.readIORef ref
              when (isNothing a) (void $ start ref c)
              go ref
            Stop -> cancel' ref >> go ref
            Kill -> cancel' ref >> C.atomically (commit c ShutDown)
            Died ->
              case cfg of
                AllowDeath -> C.atomically $ commit c ShutDown
                KeepAlive x -> do
                  sleep x
                  _ <- C.atomically $ commit c Start
                  go ref
            Reset -> do
              a <- C.readIORef ref
              when (not $ isNothing a) (cancel' ref)
              _ <- start ref c
              go ref
            _ -> go ref
    start ref c' = do
      a' <- async (app >> C.atomically (commit c' Died))
      C.writeIORef ref (Just a')
      C.atomically $ commit c' Ready
    cancel' ref = do
      mapM_ cancel =<< C.readIORef ref
      C.writeIORef ref Nothing

runControlBox :: ControlConfig -> IO () -> IO ()
runControlBox cfg action =
  etc
    ()
    (Transducer $ \s -> s & S.takeWhile (/= ShutDown))
    (boxForgetPlug (void <$> controlBox cfg action))

-- | send Start, wait for a Ready signal, run action, wait x secs, then send Quit
testBox :: IO Bool
testBox = cb
  where
    action =
      sequence_ $
      (\x -> putStrLn x >> sleep 1) . (show :: Integer -> Text) <$>
      reverse [0 .. 10]
    cb = with consoleControlBox (controlBox (KeepAlive 3) action)
  -- buff (bounded 1)
  -- ControlStart
  -- ControlReady
  -- ControlQuit

timeOut :: Double -> ControlBox m
timeOut t =
  Box <$> mempty <*> ((lift (sleep t) >> S.yield Stop) |> toEmit)
