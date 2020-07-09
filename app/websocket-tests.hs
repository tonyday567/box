{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{-

Models a websocket connection

-}

module Main where

import Box
import Control.Lens hiding (Wrapped, Unwrapped)
import Data.Generics.Labels ()
import qualified Network.WebSockets as WS
import NumHask.Prelude hiding (STM, bracket)
import Control.Monad.Conc.Class as C
import Control.Monad.Catch
import qualified Control.Concurrent.Classy.Async as C
import Options.Generic

data ConfigSocket
  = ConfigSocket
      { host :: Text,
        port :: Int,
        path :: Text
      }
  deriving (Show, Eq, Generic)

defaultConfigSocket :: ConfigSocket
defaultConfigSocket = ConfigSocket "127.0.0.1" 9160 "/"

client :: (MonadIO m) => ConfigSocket -> WS.ClientApp () -> m ()
client c app = liftIO $ WS.runClient (unpack $ c ^. #host) (c ^. #port) (unpack $ c ^. #path) app

server :: (MonadIO m) => ConfigSocket -> WS.ServerApp -> m ()
server c app = liftIO $ WS.runServer (unpack $ c ^. #host) (c ^. #port) app

con :: (MonadMask m, MonadIO m) => WS.PendingConnection -> Cont m WS.Connection
con p = Cont $ \action ->
    bracket
      (liftIO $ WS.acceptRequest p)
      (\conn -> liftIO $ WS.sendClose conn ("Bye from con!" :: Text))
      action

clientApp :: (MonadIO m, MonadConc m) =>
  Box m (Either Text Text) Text ->
  WS.Connection ->
  m ()
clientApp (Box c e) conn =
  void $
    C.race
      (receiver c conn)
      (sender (Box mempty e) conn)

serverApp ::
  WS.PendingConnection ->
  IO ()
serverApp p =
  with (con p)
  (responder
   (\x -> bool (Right $ "echo:" <> x) (Left "quit") (x=="q"))
   mempty
  )

serverIO :: IO ()
serverIO = server defaultConfigSocket serverApp

clientIO :: IO ()
clientIO =
  (client defaultConfigSocket . clientApp)
  (Box (contramap show toStdout) fromStdin)

data SocketType = Client | Responder | TestRun deriving (Eq, Read, Show, Generic)

instance ParseField SocketType

instance ParseRecord SocketType

instance ParseFields SocketType

data Opts w
  = Opts
      { apptype :: w ::: SocketType <?> "type of websocket app"
      }
  deriving (Generic)

instance ParseRecord (Opts Wrapped)

main :: IO ()
main = do
  o :: Opts Unwrapped <- unwrapRecord "example websocket apps"
  r :: Text <- case apptype o of
    Client -> show <$> clientIO
    Responder -> show <$> q serverIO
    TestRun -> show <$> testRun
  putStrLn r

-- | default websocket receiver
-- Lefts are info/debug
receiver :: (MonadIO m) =>
  Committer m (Either Text Text) ->
  WS.Connection ->
  m Bool
receiver c conn = go
  where
    go = do
      msg <- liftIO $ WS.receive conn
      case msg of
        WS.ControlMessage (WS.Close w b) ->
          commit
            c
            ( Left
                ( "receiver: received: close: " <> show w <> " " <> show b
                )
            )
        WS.ControlMessage _ -> go
        WS.DataMessage _ _ _ msg' -> do
          commit c $ Left $ "receiver: received: " <> (WS.fromDataMessage msg' :: Text)
          _ <- commit c (Right (WS.fromDataMessage msg'))
          go

-- | default websocket sender
sender ::
  (MonadIO m, WS.WebSocketsData a, Show a) =>
  Box m Text a ->
  WS.Connection ->
  m ()
sender (Box c e) conn = forever $ do
  msg <- emit e
  case msg of
    Nothing -> pure ()
    Just msg' -> do
      commit c $ "sender: sending: " <> (show msg' :: Text)
      liftIO $ WS.sendTextData conn msg'

-- | a receiver that responds based on received Text.
-- lefts are quit signals. Rights are response text.
responder :: (MonadIO m) =>
  (Text -> Either Text Text) ->
  Committer m Text ->
  WS.Connection ->
  m ()
responder f c conn = go
  where
    go = do
      msg <- liftIO $ WS.receive conn
      case msg of
        WS.ControlMessage (WS.Close _ _) -> do
          commit c "responder: normal close"
          liftIO $ WS.sendClose conn ("received close signal: responder closed." :: Text)
        WS.ControlMessage _ -> go
        WS.DataMessage _ _ _ msg' -> do
          case (f $ WS.fromDataMessage msg') of
            Left _ -> do
              commit c "responder: sender initiated close"
              liftIO $ WS.sendClose conn ("received close signal: responder closed." :: Text)
            Right r -> do
              commit c ("responder: sending" <> r)
              liftIO $ WS.sendTextData conn r
              go

q :: IO a -> IO (Either () a)
q f = race (cancelQ fromStdin) f

cancelQ :: Emitter IO Text -> IO ()
cancelQ e = do
  e' <- emit e
  case e' of
    Just "q" -> pure ()
    _ -> do
      putStrLn ("nothing happens" :: Text)
      cancelQ e

-- | test of clientApp via a cRef committer and a canned list of Text
tClient :: [Text] -> IO [Either Text Text]
tClient xs = do
  (c,r) <- cRef
  client defaultConfigSocket
    (\conn ->
       (\b -> clientApp b conn) <$.>
       (Box <$>
        pure c <*>
        fromListE (xs <> ["q"])))
  r

tClientIO :: [Text] -> IO ()
tClientIO xs =
  (client defaultConfigSocket . clientApp) <$.>
  (Box (contramap show toStdout) <$> (fromListE (xs <> ["q"])))

-- | main test run of client-server functionality
-- the code starts a server in a thread, starts the client in the main thread, and cancels the server on completion.
-- >>> testRun
-- [Left "receiver: received: echo:1",Right "echo:1",Left "receiver: received: echo:2",Right "echo:2",Left "receiver: received: echo:3",Right "echo:3",Left "receiver: received: close: 1000 \"received close signal: responder closed.\""]
testRun :: IO [Either Text Text]
testRun = do
  a <- async (server defaultConfigSocket serverApp)
  r <- tClient (show <$> [1..3::Int])
  cancel a
  pure r
