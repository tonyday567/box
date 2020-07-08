{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | queues
-- Follows [pipes-concurrency](https://hackage.haskell.org/package/pipes-concurrency)
module Box.Queue
  ( Queue (..),
    queueC,
    queueE,
    waitCancel,
    ends,
    withQE,
    withQC,
    toBox,
    toBoxM,
    liftB,
    concurrentlyLeft,
    concurrentlyRight,
  )
where

import Box.Box
import Box.Committer
import Box.Emitter
import Control.Concurrent.Classy.Async as C
import Control.Concurrent.Classy.STM as C
import Control.Monad.Catch as C
import Control.Monad.Conc.Class as C
import NumHask.Prelude hiding (STM, atomically)

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
      q <- newTBQueue (fromIntegral n)
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
      q <- newTBQueue (fromIntegral n)
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
readCheck sealed o =
  (Just <$> o)
    <|> ( do
            b <- readTVar sealed
            C.check b
            pure Nothing
        )

-- | turn a queue into a box (and a seal)
toBox ::
  (MonadSTM stm) =>
  Queue a ->
  stm (Box stm a a, stm ())
toBox q = do
  (i, o) <- ends q
  sealed <- newTVarN "sealed" False
  let seal = writeTVar sealed True
  pure
    ( Box
        (Committer (writeCheck sealed i))
        (Emitter (readCheck sealed o)),
      seal
    )

toBoxM ::
  (MonadConc m) =>
  Queue a ->
  m (Box m a a, m ())
toBoxM q = do
  (b, s) <- atomically $ toBox q
  pure (liftB b, atomically s)

-- | wait for the first action, and then cancel the second
waitCancel :: (MonadConc m) => m b -> m a -> m b
waitCancel a b =
  C.withAsync a $ \a' ->
    C.withAsync b $ \b' -> do
      a'' <- C.wait a'
      C.cancel b'
      pure a''

-- | run two actions concurrently, but wait and return on the left result.
concurrentlyLeft :: MonadConc m => m a -> m b -> m a
concurrentlyLeft left right =
  C.withAsync left $ \a ->
    C.withAsync right $ \_ ->
      C.wait a

-- | run two actions concurrently, but wait and return on the right result.
concurrentlyRight :: MonadConc m => m a -> m b -> m b
concurrentlyRight left right =
  C.withAsync left $ \_ ->
    C.withAsync right $ \b ->
      C.wait b

-- | connect a committer and emitter action via spawning a queue, and wait for both to complete.
withQC ::
  (MonadConc m) =>
  Queue a ->
  (Queue a -> m (Box m a a, m ())) ->
  (Committer m a -> m l) ->
  (Emitter m a -> m r) ->
  m l
withQC q spawner cio eio =
  C.bracket
    (spawner q)
    snd
    ( \(box, seal) ->
        concurrentlyLeft
          (cio (committer box) `C.finally` seal)
          (eio (emitter box) `C.finally` seal)
    )

-- | connect a committer and emitter action via spawning a queue, and wait for both to complete.
withQE ::
  (MonadConc m) =>
  Queue a ->
  (Queue a -> m (Box m a a, m ())) ->
  (Committer m a -> m l) ->
  (Emitter m a -> m r) ->
  m r
withQE q spawner cio eio =
  C.bracket
    (spawner q)
    snd
    ( \(box, seal) ->
        concurrentlyRight
          (cio (committer box) `C.finally` seal)
          (eio (emitter box) `C.finally` seal)
    )

-- | create an unbounded queue, returning the emitter result
queueC ::
  (MonadConc m) =>
  (Committer m a -> m l) ->
  (Emitter m a -> m r) ->
  m l
queueC cm em = withQC Unbounded toBoxM cm em

-- | create an unbounded queue, returning the emitter result
queueE ::
  (MonadConc m) =>
  (Committer m a -> m l) ->
  (Emitter m a -> m r) ->
  m r
queueE cm em = withQE Unbounded toBoxM cm em

-- | lift a box from STM
liftB :: (MonadConc m) => Box (STM m) a b -> Box m a b
liftB (Box c e) = Box (hoist atomically c) (hoist atomically e)
