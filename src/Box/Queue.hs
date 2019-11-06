{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}

-- | queues
-- Follows [pipes-concurrency](https://hackage.haskell.org/package/pipes-concurrency)
--
module Box.Queue
  ( Queue(..)
  , queue
  , queueC
  , queueE
  , queueCM
  , queueEM
  , queueCLog
  , queueELog
  , waitCancel
  , toBoxLog
  , ends
  ) where

import Box.Box
import Box.Committer
import Box.Emitter
-- import GHC.Conc
import Protolude hiding (STM, check, wait, cancel, atomically, withAsync, concurrently)
import Control.Concurrent.Classy.STM as C
import Control.Monad.Conc.Class as C
import Control.Concurrent.Classy.Async as C
import Control.Monad.Catch as C

-- | 'Queue' specifies how messages are queued
data Queue a
  = Unbounded
  | Bounded Int
  | Single
  | Latest a
  | Newest Int
  | New

-- | create a queue, returning the ends
ends :: MonadSTM stm => Queue a -> stm (a -> stm (), stm a)
ends qu =
  case qu of
    Bounded n -> do
      q <- newTBQueue n
      return (writeTBQueue q, readTBQueue q)
    Unbounded -> do
      q <- newTQueue
      return (writeTQueue q, readTQueue q)
    Single -> do
      m <- newEmptyTMVar
      return (putTMVar m, takeTMVar m)
    Latest a -> do
      t <- newTVar a
      return (writeTVar t, readTVar t)
    New -> do
      m <- newEmptyTMVar
      return (\x -> tryTakeTMVar m *> putTMVar m x, takeTMVar m)
    Newest n -> do
      q <- newTBQueue n
      let write x = writeTBQueue q x <|> (tryReadTBQueue q *> write x)
      return (write, readTBQueue q)

-- | write to a queue, checking the seal
writeCheck :: (MonadSTM stm) => TVar stm Bool -> (a -> stm ()) -> a -> stm Bool
writeCheck sealed i a = do
  b <- readTVar sealed
  if b
    then pure False
    else do
      i a
      pure True

-- | read from a queue, and retry if not sealed
readCheck :: MonadSTM stm => TVar stm Bool -> stm a -> stm (Maybe a)
readCheck sealed o = (Just <$> o) <|> (do
  b <- readTVar sealed
  C.check b
  pure Nothing)

-- | turn a queue into a box (and a seal)
toBox :: (MonadSTM stm) =>
  Queue a -> stm (Box stm a a, stm ())
toBox q = do
  (i, o) <- ends q
  sealed <- newTVarN "sealed" False
  let seal = writeTVar sealed True
  pure (Box
        (Committer (writeCheck sealed i))
        (Emitter (readCheck sealed o)),
        seal)

-- | write to a queue, checking the seal
writeCheckLog :: (Show a, MonadIO m, MonadConc m) => TVar (STM m) Bool -> (a -> (STM m) ()) -> a -> m Bool
writeCheckLog sealed i a = do
  b <- atomically $ readTVar sealed
  if b
    then do
      putStrLn ("writeCheckLog sealed" :: Text)
      pure False
    else do
      putStrLn $ "writeCheckLog writing: " <> (show a:: Text)
      atomically $ i a
      pure True

-- | read from a queue, and retry if not sealed
readCheckLog :: (Show a, MonadIO m, MonadConc m) => TVar (STM m) Bool -> (STM m) a -> m (Maybe a)
readCheckLog sealed o = do
  o' <- atomically o
  putStrLn $ "readCheckLog reading" <> (show o' :: Text)
  atomically (Just <$> o <|> do
                            b <- readTVar sealed
                            C.check b
                            pure Nothing)

toBoxM :: (MonadConc m) =>
  Queue a -> m (Box m a a, m ())
toBoxM q = do
  (i, o) <- atomically $ ends q
  sealed <- atomically $ newTVarN "sealed" False
  let seal = atomically $ writeTVar sealed True
  pure (Box
        (Committer (atomically . writeCheck sealed i))
        (Emitter (atomically $ readCheck sealed o)),
        seal)

toBoxLog :: (Show a, MonadIO m, MonadConc m) =>
  Queue a -> m (Box m a a, m ())
toBoxLog q = do
  (i, o) <- atomically $ ends q
  sealed <- atomically $ newTVarN "sealed" False
  initSeal <- atomically $ readTVar sealed
  putStrLn $ "toBoxLog: seal: " <> (show initSeal :: Text)
  let seal = atomically $ writeTVar sealed True
  pure (Box
        (Committer (writeCheckLog sealed i))
        (Emitter (readCheckLog sealed o)),
        seal)

-- | wait for the first action, and then cancel the second
waitCancel :: (MonadConc m) => m b -> m a -> m b
waitCancel a b =
  withAsync a $ \a' ->
    withAsync b $ \b' -> do
      a'' <- wait a'
      cancel b'
      pure a''

-- | connect a committer and emitter action via spawning a queue, and wait for both to complete.
withQ :: (MonadConc m) =>
     Queue a
  -> (Queue a -> (STM m) (Box (STM m) a a, (STM m) ()))
  -> (Committer (STM m) a -> m l)
  -> (Emitter (STM m) a -> m r)
  -> m (l, r)
withQ q spawner cio eio =
  C.bracket
    (atomically $ spawner q)
    (\(_, seal) -> atomically seal)
    (\(box, seal) ->
       concurrently
         (cio (committer box) `C.finally` atomically seal)
         (eio (emitter box) `C.finally` atomically seal))

-- | connect a committer and emitter action via spawning a queue, and wait for both to complete.
withQM :: (MonadConc m) =>
     Queue a
  -> (Queue a -> m (Box m a a, m ()))
  -> (Committer m a -> m l)
  -> (Emitter m a -> m r)
  -> m (l, r)
withQM q spawner cio eio =
  C.bracket
    (spawner q)
    snd
    (\(box, seal) ->
       concurrently
         (cio (committer box) `C.finally` seal)
         (eio (emitter box) `C.finally` seal))

-- | connect a committer and emitter action via spawning a queue, and wait for both to complete.
withQMLog :: (MonadConc m, MonadIO m) =>
     Queue a
  -> (Queue a -> m (Box m a a, m ()))
  -> (Committer m a -> m l)
  -> (Emitter m a -> m r)
  -> m (l, r)
withQMLog q spawner cio eio =
  C.bracket
    (spawner q)
    snd
    (\(box, seal) ->
       concurrently
         (cio (committer box) `C.finally` (putStrLn ("committer sealed" :: Text) >> seal))
         (eio (emitter box) `C.finally` (putStrLn ("emitter sealed" :: Text) >> seal)))

-- | create an unbounded queue
queue ::
  (MonadConc m) =>
  (Committer (STM m) a -> m l) ->
  (Emitter (STM m) a -> m r) ->
  m (l, r)
queue = withQ Unbounded toBox

-- | create an unbounded queue, returning the emitter result
queueE ::
  (MonadConc m) =>
  (Committer (STM m) a -> m l) ->
  (Emitter (STM m) a -> m r) ->
  m r
queueE cm em = snd <$> withQ Unbounded toBox cm em

-- | create an unbounded queue, returning the committer result
queueC ::
  (MonadConc m) =>
  (Committer (STM m) a -> m l) ->
  (Emitter (STM m) a -> m r) ->
  m l
queueC cm em = fst <$> withQ Unbounded toBox cm em


-- | create an unbounded queue, returning the emitter result
queueCM ::
  (MonadConc m) =>
  (Committer m a -> m l) ->
  (Emitter m a -> m r) ->
  m l
queueCM cm em = fst <$> withQM Unbounded toBoxM cm em

-- | create an unbounded queue, returning the emitter result
queueEM ::
  (MonadConc m) =>
  (Committer m a -> m l) ->
  (Emitter m a -> m r) ->
  m r
queueEM cm em = snd <$> withQM Unbounded toBoxM cm em

-- | create an unbounded queue, returning the emitter result
queueELog ::
  (Show a, MonadConc m, MonadIO m) =>
  (Committer m a -> m l) ->
  (Emitter m a -> m r) ->
  m r
queueELog cm em = snd <$> withQMLog Unbounded toBoxLog
  (\c -> putStrLn ("cm acted" :: Text) >> cm c)
  (\c -> putStrLn ("em acted" :: Text) >> em c)

-- | create an unbounded queue, returning the committer result
queueCLog ::
  (Show a, MonadConc m, MonadIO m) =>
  (Committer m a -> m l) ->
  (Emitter m a -> m r) ->
  m l
queueCLog cm em = fst <$> withQM Unbounded toBoxLog cm em
