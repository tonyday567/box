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
import NumHask.Prelude hiding (STM, bracket)
import Control.Monad.Conc.Class as C
import Control.Monad.Catch
import qualified Control.Concurrent.Classy.Async as C

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
  Box.with (con p)
  (responder
   (\x -> bool (Right $ "echo:" <> x) (Left "quit") (x=="q"))
   mempty
  )

server' :: IO ()
server' = server defaultConfigSocket serverApp

client' :: IO ()
client' = (client defaultConfigSocket . clientApp) <$.> (Box <$> pure showStdout <*> (fromListE ["a", "b", "q", "x"]))

-- | TODO: is broken compared with client'
client'' :: [Text] -> IO [Either Text Text]
client'' xs = do
  ref <- C.newIORef []
  client defaultConfigSocket (clientState xs ref)
  C.readIORef ref

clientState :: (MonadIO m, MonadConc m) => [Text] -> IORef m [Either Text Text] -> WS.Connection -> m ()
clientState xs ref conn = do
  (res, res') <- flip execStateT ([],xs) $ clientApp (Box (hoist (zoom _1) stateC) (hoist (zoom _2) stateE)) conn
  putStrLn (show res' :: Text)
  C.writeIORef ref res

main :: IO ()
main = do
  r <- q server'
  putStrLn (show r :: Text)

testRun :: IO ()
testRun = do
  withAsync
    (server defaultConfigSocket serverApp)
    (\a -> sleep 10 >> cancel a)

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
