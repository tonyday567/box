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
  , waitCancel
  ) where

import Box.Box
import Box.Committer
import Box.Emitter
import GHC.Conc
import Protolude hiding ((<>), STM, check, wait, cancel, atomically, withAsync, concurrently)
import Control.Concurrent.Classy.STM as C
import Control.Monad.Conc.Class as C hiding (spawn)
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

writeCheck :: (MonadSTM stm) => TVar stm Bool -> (a -> stm ()) -> a -> stm Bool
writeCheck sealed i a = do
  b <- readTVar sealed
  if b
    then pure False
    else do
      i a
      pure True

readCheck :: MonadSTM stm => TVar stm Bool -> stm a -> stm (Maybe a)
readCheck sealed o = (Just <$> o) <|> (do
  b <- readTVar sealed
  C.check b
  pure Nothing)

-- | copied shamefully from pipes-concurrency
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

queue ::
  (MonadConc m) =>
  (Committer (STM m) a -> m l) ->
  (Emitter (STM m) a -> m r) ->
  m (l, r)
queue = withQ Unbounded toBox

queueE ::
  (MonadConc m) =>
  (Committer (STM m) a -> m l) ->
  (Emitter (STM m) a -> m r) ->
  m r
queueE cm em = snd <$> withQ Unbounded toBox cm em

queueC ::
  (MonadConc m) =>
  (Committer (STM m) a -> m l) ->
  (Emitter (STM m) a -> m r) ->
  m l
queueC cm em = fst <$> withQ Unbounded toBox cm em
