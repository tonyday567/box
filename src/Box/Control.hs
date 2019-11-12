{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Box.Control
  ( ControlRequest (..),
    ControlResponse (..),
    ControlBox,
    ControlConfig (..),
    defaultControlConfig,
    consoleControlBox,
    parseControlRequest,
    controlBox,
    -- runControlBox,
    testBox,
    timeOut,
  )
where

import Box
import Control.Applicative
import Control.Concurrent.Async
import Control.Lens hiding ((|>))
import Control.Monad
import Control.Monad.Conc.Class as C
import qualified Data.Attoparsec.Text as A
import Data.Data
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text (Text)
import GHC.Generics
import qualified Streaming.Prelude as S
import Text.Read (readMaybe)
import Data.Functor
import Control.Monad.Trans.Class
import Data.Bool
import Data.Maybe

data ControlRequest
  = Check -- check for existence
  | Stop -- stop (without shutting down)
  | Quit -- stop, quit & cancel thread
  | Start -- start (if not yet started)
  | Reset -- stop and start (potentially cancelling a previous instance)
  | Kill -- immediately exit
  deriving (Show, Read, Eq, Data, Typeable, Generic)

data ControlResponse
  = ShutDown -- action died
  | On Bool -- are we live?
  | Log Text
  deriving (Show, Read, Eq, Data, Typeable, Generic)

type ControlBox m = (MonadConc m) => Cont m (Box (STM m) ControlResponse ControlRequest)

data ControlConfig
  = KeepAlive Double
  | AllowDeath
  deriving (Show, Eq)

defaultControlConfig :: ControlConfig
defaultControlConfig = AllowDeath

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

parseControlRequest :: A.Parser ControlRequest
parseControlRequest =
  A.string "q" $> Stop
    <|> A.string "s" $> Start
    <|> A.string "x" $> Quit
    <|> A.string "c" $> Check
    <|> A.string "r" $> Reset
    <|> do
      res <- readMaybe . Text.unpack <$> A.takeText
      case res of
        Nothing -> mzero
        Just a -> return a

-- | an effect that can be started and stopped
controlBox ::
  IO a ->
  Box (STM IO) ControlResponse ControlRequest ->
  IO ()
controlBox app (Box c e) = do
  ref' <- C.newIORef Nothing
  _ <- C.atomically (commit c (On False))
  _ <- go ref'
  Text.putStrLn ("after go ref'" :: Text)
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
            Stop -> cancel' ref c >> go ref
            Quit -> cancel' ref c >> C.atomically (commit c ShutDown)
            Reset -> do
              a <- C.readIORef ref
              unless (isNothing a) (void $ cancel' ref c)
              _ <- start ref c
              go ref
            _ -> go ref
    start ref c' = do
      a' <- async (app >> C.atomically (commit c' (On False)))
      C.writeIORef ref (Just a')
      C.atomically $ commit c' (On True)
    cancel' ref c' = do
      mapM_ cancel =<< C.readIORef ref
      C.writeIORef ref Nothing
      C.atomically $ commit c' (On False)

-- | send Start, wait for a Ready signal, run action, wait x secs, then send Quit
testBox :: IO ()
testBox = cb
  where
    action =
      replicateM_ 3 (sleep 1 >> Text.putStrLn ("beep" :: Text))
    cb = with consoleControlBox (controlBox action)

timeOut :: Double -> ControlBox m
timeOut t =
  Box <$> mempty <*> ((lift (sleep t) >> S.yield Stop) & toEmit)
