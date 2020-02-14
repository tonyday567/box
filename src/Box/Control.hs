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
    ControlBox,
    ControlConfig (..),
    defaultControlConfig,
    consoleControlBox,
    parseControlRequest,
    parseControlRequest',
    controlBox,
    testBox,
    beep,
    timeOut,
  )
where

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
import Text.Read (readMaybe)

-- | request ADT
data ControlRequest
  = Check -- check for existence
  | Start -- start (if not yet started) idempotent
  | Stop -- cancel (without shutting down) idempotent
  | Reset -- stop and start (potentially cancelling a previous instance)
  | Quit -- stop & shutdown
  deriving (Show, Read, Eq, Data, Typeable, Generic)

-- | Parse command line requests
parseControlRequest :: A.Parser ControlRequest
parseControlRequest =
  A.string "c" $> Check
    <|> A.string "s" $> Start
    <|> A.string "q" $> Stop
    <|> A.string "r" $> Reset
    <|> A.string "x" $> Quit
    <|> do
      res <- readMaybe . Text.unpack <$> A.takeText
      case res of
        Nothing -> mzero
        Just a -> return a

-- | Parse command line requests or interpret as Text
parseControlRequest' :: A.Parser (Either ControlRequest Text)
parseControlRequest' =
  A.string "c" $> Left Check
    <|> A.string "s" $> Left Start
    <|> A.string "q" $> Left Stop
    <|> A.string "r" $> Left Reset
    <|> A.string "x" $> Left Quit
    <|> Right <$> A.takeText

data Toggle = On | Off deriving (Show, Read, Eq, Generic)

-- | response ADT
data ControlResponse
  = ShutDown -- shutdown
  | Status (Toggle, Int) -- on/off and number restarts left
  | Info Text
  deriving (Show, Read, Eq, Generic)

-- | A 'Box' that communicates via 'ControlRequest' and 'ControlResponse'
type ControlBox m = (MonadConc m) => Cont m (Box (STM m) ControlResponse ControlRequest)

-- |
data ControlConfig
  = ControlConfig
      { restarts :: Int,
        autoStart :: Bool,
        autoRestart :: Bool,
        autoRestartTimeout :: Double
      }
  deriving (Show, Eq, Ord)

-- | Default is zero restarts, manual start, no autorestart and sleep for a second on autorestarting.
defaultControlConfig :: ControlConfig
defaultControlConfig = ControlConfig 0 False False 1

-- | a command-line control box.
consoleControlBox :: ControlBox IO
consoleControlBox =
  Box
    <$> ( contramap (Text.pack . show)
            <$> (cStdout 1000 :: Cont IO (Committer (STM IO) Text))
        )
    <*> ( emap (pure . either (const Nothing) Just)
            <$> ( eParse parseControlRequest
                    <$> eStdin 1000
                )
        )

data ControlBoxState a = CBS {actionThread :: Maybe (Async ()), restartsLeft :: Int}

-- | an effect that can be started, stopped and restarted (a limited number of times)
controlBox ::
  ControlConfig ->
  IO a ->
  Box (STM IO) ControlResponse ControlRequest ->
  IO ()
controlBox (ControlConfig restarts' autostart autorestart t) app (Box c e) = do
  ref <- C.newIORef (CBS Nothing restarts')
  shut <- atomically $ newTVar False
  when autostart (start ref shut)
  race_
    (go ref shut)
    (shutme shut >> cancelRef ref)
  where
    cancelRef r = do
      (CBS a _) <- readIORef r
      maybe (pure ()) cancel a
    shutme s =
      atomically $ do
        signal <- readTVar s
        check signal
    status r = do
      s <- C.readIORef r
      C.atomically
        ( void $
            commit
              c
              (Status (bool Off On (isJust (actionThread s)), restartsLeft s))
        )
    loopApp r s app' = do
      cfg@(CBS _ n) <- readIORef r
      writeIORef r (cfg {restartsLeft = n - 1})
      _ <- app'
      shutdownIfNoRestarts r s
      when autorestart $ do
        sleep t
        loopApp r s app'
    start r s = do
      (CBS a _) <- readIORef r
      when (isNothing a) $ do
        a' <-
          async
            ( do
                loopApp r s app
                cfg <- readIORef r
                writeIORef r (cfg {actionThread = Nothing})
            )
        link a'
        cfg <- readIORef r
        writeIORef r (cfg {actionThread = Just a'})
    stop r = do
      cfg@(CBS a _) <- C.readIORef r
      maybe
        (pure ())
        ( \x -> do
            cancel x
            C.writeIORef r (cfg {actionThread = Nothing})
        )
        a
    shutdown =
      void $ commit (liftC c) ShutDown
    shutdownIfNoRestarts r s = do
      (CBS _ n) <- C.readIORef r
      bool
        ( do
            atomically $ writeTVar s True
            shutdown
        )
        (pure ())
        (n >= 0)
    go r s = do
      status r
      shutdownIfNoRestarts r s
      msg <- C.atomically $ emit e
      case msg of
        Nothing -> go r s
        Just msg' ->
          case msg' of
            Check -> do
              status r
              go r s
            Start -> do
              start r s
              go r s
            Stop -> do
              stop r
              go r s
            Quit -> stop r >> shutdown
            Reset -> stop r >> start r s >> go r s

beep :: Int -> Int -> Double -> IO ()
beep m x s = when (x <= m) (sleep s >> Text.putStrLn ("beep " <> Text.pack (show x)) >> beep m (x + 1) s)

-- |
-- >>> testBox (ControlConfig 1 True True 0.5) 2.3 (beep 3 1 0.5)
-- Status (On,0)
-- beep 1
-- beep 2
-- beep 3
-- ShutDown
testBox :: ControlConfig -> Double -> IO () -> IO ()
testBox cfg t effect = with (consoleControlBox <> timeOut t) (controlBox cfg effect)

-- | A box with a self-destruct timer.
timeOut :: Double -> ControlBox m
timeOut t =
  Box <$> mempty <*> ((lift (sleep t) >> S.yield Quit) & toEmit)

