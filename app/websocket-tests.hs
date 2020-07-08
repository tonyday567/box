{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

{-

FIXME:

Models a websocket connection:

The client socket reads (Either ControlRequest Text) from stdin.

The server socket processes Left ControlRequests and echoes Right text messages back to the client.

The client prints received messages to stdout.

-}

module Main where

import Box
import Control.Lens
import Data.Generics.Labels ()
import qualified Network.WebSockets as WS
import NumHask.Prelude hiding (STM)

data ConfigSocket
  = ConfigSocket
      { host :: Text,
        port :: Int,
        path :: Text
      }
  deriving (Show, Eq, Generic)

defaultConfigSocket :: ConfigSocket
defaultConfigSocket = ConfigSocket "127.0.0.1" 9160 "/"

client :: ConfigSocket -> WS.ClientApp () -> IO ()
client c = WS.runClient (unpack $ c ^. #host) (c ^. #port) (unpack $ c ^. #path)

server :: ConfigSocket -> WS.ServerApp -> IO ()
server c = WS.runServer (unpack $ c ^. #host) (c ^. #port)

con :: WS.PendingConnection -> Cont IO WS.Connection
con p = Cont $
    bracket
      (WS.acceptRequest p)
      (\conn -> WS.sendClose conn ("Bye from con!" :: Text))

clientApp ::
  Box IO (Either Text Text) Text ->
  WS.ClientApp ()
clientApp (Box c e) conn =
  void $
    race
      (receiver c conn)
      (sender e conn)

serverApp ::
  WS.PendingConnection ->
  IO ()
serverApp p = Box.with (con p) (responder (\x -> pure ("echo:" <> x)))

server' :: IO ()
server' = do
  withAsync
    (server defaultConfigSocket serverApp)
    (\_ -> cancelQ readStdin)

client' :: IO ()
client' = (client defaultConfigSocket . clientApp) (Box showStdout readStdin)

main :: IO ()
main = server'

testRun :: IO ()
testRun = do
  withAsync
    (server defaultConfigSocket serverApp)
    (\a -> sleep 10 >> cancel a)

-- | default websocket receiver
-- Lefts are info/debug
receiver ::
  Committer IO (Either Text Text) ->
  WS.Connection ->
  IO Bool
receiver c conn = go
  where
    go = do
      msg <- WS.receive conn
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
          putStrLn $ "receiver: received: " <> (WS.fromDataMessage msg' :: Text)
          _ <- commit c (Right (WS.fromDataMessage msg'))
          go

-- | default websocket sender
sender ::
  (WS.WebSocketsData a, Show a) =>
  Emitter IO a ->
  WS.Connection ->
  IO ()
sender e conn = forever $ do
  msg <- emit e
  case msg of
    Nothing -> pure ()
    Just msg' -> do
      putStrLn $ "sender: sending: " <> (show msg' :: Text)
      WS.sendTextData conn msg'

-- | a receiver that responds based on received Text
responder ::
  (Text -> IO Text) ->
  WS.Connection ->
  IO ()
responder f conn = go
  where
    go = do
      msg <- WS.receive conn
      case msg of
        WS.ControlMessage (WS.Close _ _) -> do
          WS.sendClose conn ("received close signal: responder closed." :: Text)
        WS.ControlMessage _ -> go
        WS.DataMessage _ _ _ msg' -> do
          r <- (f $ WS.fromDataMessage msg')
          WS.sendTextData conn r
          go

cancelQ :: Emitter IO Text -> IO ()
cancelQ e = do
  e' <- emit e
  case e' of
    Just "q" -> pure ()
    _ -> do
      putStrLn ("nothing happens" :: Text)
      cancelQ e
