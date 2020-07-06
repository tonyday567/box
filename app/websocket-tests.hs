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
import Box.Control
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

-- | default websocket receiver
receiver ::
  Committer IO (Either ControlResponse Text) ->
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
                ( Info $
                    "receiver: received: close: " <> show w <> " " <> show b
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

clientApp ::
  Box IO (Either ControlResponse Text) Text ->
  WS.ClientApp ()
clientApp (Box c e) conn =
  void $
    concurrently
      (receiver c conn)
      (sender e conn)

clientApp' ::
  Box IO (Either ControlResponse Text) Text ->
  WS.ClientApp ()
clientApp' (Box c e) conn =
  void $
    race
      (receiver c conn)
      (sender e conn)

-- | single uncontrolled client
clientBox ::
  ConfigSocket ->
  IO ()
clientBox cfg =
  Box.with (liftB <$> (Box <$> showStdout <*> readStdin)) (client cfg . clientApp)

clientBox' ::
  ConfigSocket ->
  IO ()
clientBox' cfg =
  Box.with ((Box <$> (liftC <$> showStdout) <*> (liftE <$> eStdin'))) (client cfg . clientApp')

-- | a receiver that immediately sends a response
responder ::
  (Text -> Either ControlResponse Text) ->
  WS.Connection ->
  IO ()
responder f conn = go
  where
    go = do
      msg <- WS.receive conn
      case msg of
        WS.ControlMessage (WS.Close _ _) -> do
          WS.sendClose conn ("returning close signal" :: Text)
        WS.ControlMessage _ -> go
        WS.DataMessage _ _ _ msg' ->
          case (f $ WS.fromDataMessage msg') of
            Right t -> do
              WS.sendTextData conn t
              putStrLn $ ("responder: received: " <> WS.fromDataMessage msg' :: Text)
              go
            Left ShuttingDown -> do
              WS.sendClose conn ("received ShuttingDown signal" :: Text)
              putStrLn $ ("responder: closing" :: Text)
            Left _ -> go

-- | a receiver that responds based on received Text
responder' ::
  (Text -> IO Text) ->
  WS.Connection ->
  IO ()
responder' f conn = go
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

serverApp ::
  WS.PendingConnection ->
  IO ()
serverApp p = Box.with (con p) (responder' (\x -> pure ("echo:" <> x)))

-- | controlled server
serverBox ::
  ConfigSocket ->
  IO ()
serverBox cfg =
  Box.with
    (Box <$> showStdout <*> readStdin)
    (controlBox (ControlConfig 3 True Nothing True) (server cfg serverApp))

server' :: IO ()
server' = do
  withAsync
    (server defaultConfigSocket serverApp)
    (\_ -> Box.with (liftE <$> eStdin 5) cancelQ)

testServer :: IO ()
testServer = serverBox defaultConfigSocket

testClient :: IO ()
testClient = clientBox' defaultConfigSocket


cancelQ :: Emitter IO Text -> IO ()
cancelQ e = do
  e' <- emit e
  case e' of
    Just "q" -> pure ()
    _ -> do
      putStrLn ("nothing happens" :: Text)
      cancelQ e

main :: IO ()
main = server'

testRun :: IO ()
testRun = do
  withAsync
    (server defaultConfigSocket serverApp)
    (\a -> sleep 10 >> cancel a)

