{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}

module Etc.Action
  ( ActionComm(..)
  , ControlComm(..)
  , ActionBox
  , ActionConfig(..)
  , consoleActionBox
  , parseActionComms
  , controlBox
  , runControlBox
  , testBox
  , timeOut
  ) where

import Control.Applicative
import Control.Category
import Control.Concurrent.Async
import Control.Lens hiding ((|>))
import Control.Monad
import Etc.Managed
import Data.Data
import Data.Default
import Data.IORef
import Data.Semigroup
import Etc
import Flow
import GHC.Generics 
import Protolude hiding ((.))
import qualified Data.Text as Text
import qualified Streaming.Prelude as S
import qualified Data.Attoparsec.Text as A
import Text.Read (readMaybe)
import Etc.Time (sleep)

data ActionComm
  = ActionReady -- action is ready
  | ActionCheck -- check to see if action exists
  | ActionDied -- action died (of its own accord)
  | ActionStop -- stop current action (without shutting down)
  | ActionKill -- quit the action (cancel thread)
  | ActionShutDown -- thread successfully cancelled - safe to shutdown app
  | ActionStart -- start the action (if no action exists)
  | ActionReset -- start the action (potentially cancelling a previous action)
  | ActionOn Bool -- is there an action up
  | ActionLog Text
  deriving (Show, Read, Eq, Data, Typeable, Generic)

data ControlComm
  = Start
  | Stop
  | Reset
  | Destroy
  | Exists
  | Ready
  | Closed
  | NoOpCC
  deriving (Show, Read, Eq, Data, Typeable, Generic)

type ActionBox = Managed IO (Box STM ActionComm ActionComm)

data ActionConfig = KeepAlive Double | AllowDeath deriving (Show, Eq)

instance Default ActionConfig where
  def = AllowDeath

consoleActionBox :: ActionBox
consoleActionBox =
  Box <$>
  (contramap show <$> (cStdout 1000 unbounded :: Managed IO (Committer STM Text))) <*>
  eParse parseActionComms (eStdin 1000 unbounded)

parseActionComms :: A.Parser ActionComm
parseActionComms =
  A.string "q" *> return ActionStop <|>
  A.string "s" *> return ActionStart <|>
  A.string "x" *> return ActionKill <|>
  do
    res <- readMaybe . Text.unpack <$> A.takeText
    case res of
      Nothing -> mzero
      Just a -> return a

-- | an effect that can be started and stopped
-- committer is an existence test
controlBox ::
  ActionConfig ->
  IO () ->
  Box STM ActionComm ActionComm ->
  IO Bool
controlBox cfg app (Box c e) = do
  ref' <- newIORef Nothing
  go ref'
  where
    go ref = do
      msg <- atomically $ emit e
      case msg of
        Nothing -> go ref
        Just msg' ->
          case msg' of
            ActionCheck -> do
              a <- readIORef ref
              _ <- atomically $ commit c $ ActionOn (bool True False (isNothing a))
              go ref
            ActionStart -> do
              a <- readIORef ref
              when (isNothing a) (void $ start ref c)
              go ref
            ActionStop ->
              cancel' ref >> go ref
            ActionKill -> cancel' ref >> atomically (commit c ActionShutDown)
            ActionDied ->
              case cfg of
                AllowDeath -> atomically $ commit c ActionShutDown
                KeepAlive x -> do
                  sleep x
                  _ <- atomically $ commit c ActionStart
                  go ref
            ActionReset -> do
              a <- readIORef ref
              when (not $ isNothing a) (cancel' ref)
              _ <- start ref c
              go ref
            _ -> go ref
    start ref c' = do
      a' <- async (app >> atomically (commit c' ActionDied))
      writeIORef ref (Just a')
      atomically $ commit c' ActionReady
    cancel' ref = do
      mapM_ cancel =<< readIORef ref
      writeIORef ref Nothing

runControlBox :: ActionConfig -> IO () -> IO ()
runControlBox cfg action =
  etc ()
  (Transducer $ \s -> s & S.takeWhile (/= ActionShutDown))
  (buffBoxForget (bounded 1) (bounded 1) (void <$> controlBox cfg action))

-- | send Start, wait for a Ready signal, run action, wait x secs, then send Quit
testBox :: IO Bool
testBox = cb where
  action = sequence_ $
    (\x -> putStrLn x >> sleep 1) .
    (show :: Integer -> Text) <$>
    reverse [0..10]
  cb = with consoleActionBox (controlBox (KeepAlive 3) action)

  -- buff (bounded 1)
  -- ActionStart
  -- ActionReady
  -- ActionQuit

timeOut :: Double -> ActionBox
timeOut t = Box <$> mempty <*>
  ((lift (sleep t) >> S.yield ActionStop) |> toEmit (bounded 1))
