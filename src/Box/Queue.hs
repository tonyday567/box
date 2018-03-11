{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | queues
-- Follows [pipes-concurrency](https://hackage.haskell.org/package/pipes-concurrency)
--
module Box.Queue
  ( Queue(..)
  , queue
  , queueC
  , queueE
  , queueIO
  , waitCancel
  ) where

import Box.Box
import Box.Committer
import Box.Emitter
import GHC.Conc
import Protolude hiding ((.), (<>))
import qualified Control.Concurrent.STM as S

-- | 'Queue' specifies how messages are queued
data Queue a
  = Unbounded
  | Bounded Int
  | Single
  | Latest a
  | Newest Int
  | New

ends :: Queue a -> STM (a -> STM (), STM a)
ends buffer =
  case buffer of
    Bounded n -> do
      q <- S.newTBQueue n
      return (S.writeTBQueue q, S.readTBQueue q)
    Unbounded -> do
      q <- S.newTQueue
      return (S.writeTQueue q, S.readTQueue q)
    Single -> do
      m <- S.newEmptyTMVar
      return (S.putTMVar m, S.takeTMVar m)
    Latest a -> do
      t <- S.newTVar a
      return (S.writeTVar t, S.readTVar t)
    New -> do
      m <- S.newEmptyTMVar
      return (\x -> S.tryTakeTMVar m *> S.putTMVar m x, S.takeTMVar m)
    Newest n -> do
      q <- S.newTBQueue n
      let write x = S.writeTBQueue q x <|> (S.tryReadTBQueue q *> write x)
      return (write, S.readTBQueue q)

-- | copied shamefully from pipes-concurrency
spawn :: Queue a -> IO (Box STM a a, IO ())
spawn q = do
  (write, read) <- atomically $ ends q
  sealed <- S.newTVarIO False
  let seal = S.atomically $ S.writeTVar sealed True
  rSend <- S.newTVarIO ()
  void $ S.mkWeakTVar rSend seal
  rRecv <- S.newTVarIO ()
  void $ S.mkWeakTVar rRecv seal
  let sendOrEnd a = do
        b <- S.readTVar sealed
        if b
          then return False
          else do
            write a
            return True
      readOrEnd =
        (Just <$> read) <|>
        (do b <- S.readTVar sealed
            S.check b
            return Nothing)
      _send a = sendOrEnd a <* S.readTVar rSend
      _recv = readOrEnd <* S.readTVar rRecv
  return (Box (Committer _send) (Emitter _recv), seal)

spawnIO :: Queue a -> IO (Box IO a a, IO ())
spawnIO q = fmap (\(a, b) -> (liftB a, b)) (spawn q)

-- | wait for the first action, and then cancel the second
waitCancel :: IO b -> IO a -> IO b
waitCancel a b =
  withAsync a $ \a' ->
    withAsync b $ \b' -> do
      a'' <- wait a'
      cancel b'
      pure a''

-- | connect a committer and emitter action via spawning a queue, and wait for both to complete.
withQ ::
     Queue a
  -> (Queue a -> IO (Box m a a, IO ()))
  -> (Committer m a -> IO l)
  -> (Emitter m a -> IO r)
  -> IO (l, r)
withQ q spawner cio eio =
  bracket
    (spawner q)
    (\(_, seal) -> seal)
    (\(box, seal) ->
       concurrently
         (cio (committer box) `finally` seal)
         (eio (emitter box) `finally` seal))

queue :: (Committer STM a -> IO l) -> (Emitter STM a -> IO r) -> IO (l, r)
queue = withQ Unbounded spawn

queueE :: (Committer STM a -> IO l) -> (Emitter STM a -> IO r) -> IO r
queueE cio eio = snd <$> withQ Unbounded spawn cio eio

queueC :: (Committer STM a -> IO l) -> (Emitter STM a -> IO r) -> IO l
queueC cio eio = fst <$> withQ Unbounded spawn cio eio

queueIO :: (Committer IO a -> IO l) -> (Emitter IO a -> IO r) -> IO (l, r)
queueIO = withQ Unbounded spawnIO
