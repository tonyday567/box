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
import Control.Exception (bracket)
import Control.Lens
import Control.Monad.Managed
import Data.Generics.Labels ()
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics
import qualified Network.WebSockets as WS
import Protolude hiding (STM)

data ConfigSocket
  = ConfigSocket
      { host :: Text,
        port :: Int
      }
  deriving (Show, Eq, Generic)

defaultConfigSocket :: ConfigSocket
defaultConfigSocket = ConfigSocket "127.0.0.1" 9160

client :: ConfigSocket -> WS.ClientApp () -> IO ()
client c = WS.runClient (Text.unpack $ c ^. #host) (c ^. #port) "/"

server :: ConfigSocket -> WS.ServerApp -> IO ()
server c = WS.runServer (Text.unpack $ c ^. #host) (c ^. #port)

mconn :: WS.PendingConnection -> Managed WS.Connection
mconn p =
  managed $
    bracket
      (WS.acceptRequest p)
      (\conn -> WS.sendClose conn ("Bye from mconn!" :: Text))

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
                    "received close message: " <> show w <> " " <> show b
                )
            )
        WS.ControlMessage _ -> go
        WS.DataMessage _ _ _ msg' -> commit c (Right (WS.fromDataMessage msg')) >> go

-- | default websocket sender
sender ::
  WS.WebSocketsData a =>
  Emitter IO a ->
  WS.Connection ->
  IO ()
sender e conn = forever $ do
  msg <- emit e
  case msg of
    Nothing -> pure ()
    Just msg' -> WS.sendTextData conn msg'

clientApp ::
  Box IO (Either ControlResponse Text) Text ->
  WS.ClientApp ()
clientApp (Box c e) conn =
  void $
    concurrently
      (receiver c conn)
      (sender e conn)

-- | single uncontrolled client
clientBox ::
  ConfigSocket ->
  IO ()
clientBox cfg =
  Box.with (liftB <$> (Box <$> showStdout <*> readStdin)) (client cfg . clientApp)

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
          go
        WS.ControlMessage _ -> go
        WS.DataMessage _ _ _ msg' -> WS.sendTextData conn (show $ f $ WS.fromDataMessage msg' :: Text)

serverApp ::
  WS.PendingConnection ->
  IO ()
serverApp p = Control.Monad.Managed.with (mconn p) (responder Right)

-- | controlled server
serverBox ::
  ConfigSocket ->
  IO ()
serverBox cfg =
  Box.with
    (Box <$> showStdout <*> readStdin)
    (controlBox defaultControlConfig (server cfg serverApp))

main :: IO ()
main = pure ()
