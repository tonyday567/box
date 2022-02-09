{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | STM Queues, based originally on [pipes-concurrency](https://hackage.haskell.org/package/pipes-concurrency)
module Box.Queue
  ( Queue (..),
    queueL,
    queueR,
    queue,
    fromAction,
    emitQ,
    commitQ,
  )
where

import Box.Box
import Box.Committer
import Box.Codensity
import Box.Emitter
import Control.Applicative
import Control.Concurrent.Classy.Async as C
import Control.Concurrent.Classy.STM as C
import Control.Monad.Catch as C
import Control.Monad.Conc.Class as C
import Prelude
import Box.Functor

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Box
-- >>> import Prelude

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
toBoxSTM ::
  (MonadSTM stm) =>
  Queue a ->
  stm (Box stm a a, stm ())
toBoxSTM q = do
  (i, o) <- ends q
  sealed <- newTVarN "sealed" False
  let seal = writeTVar sealed True
  pure
    ( Box
        (Committer (writeCheck sealed i))
        (Emitter (readCheck sealed o)),
      seal
    )

-- | turn a queue into a box (and a seal), and lift from stm to the underlying monad.
toBoxM ::
  (MonadConc m) =>
  Queue a ->
  m (Box m a a, m ())
toBoxM q = do
  (b, s) <- atomically $ toBoxSTM q
  pure (liftB b, atomically s)

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

-- | connect a committer and emitter action via spawning a queue, and wait for the Committer action to complete.
withQL ::
  (MonadConc m) =>
  Queue a ->
  (Queue a -> m (Box m a a, m ())) ->
  (Committer m a -> m l) ->
  (Emitter m a -> m r) ->
  m l
withQL q spawner cio eio =
  C.bracket
    (spawner q)
    snd
    ( \(box, seal) ->
        concurrentlyLeft
          (cio (committer box) `C.finally` seal)
          (eio (emitter box) `C.finally` seal)
    )

-- | connect a committer and emitter action via spawning a queue, and wait for the Emitter action to complete.
withQR ::
  (MonadConc m) =>
  Queue a ->
  (Queue a -> m (Box m a a, m ())) ->
  (Committer m a -> m l) ->
  (Emitter m a -> m r) ->
  m r
withQR q spawner cio eio =
  C.bracket
    (spawner q)
    snd
    ( \(box, seal) ->
        concurrentlyRight
          (cio (committer box) `C.finally` seal)
          (eio (emitter box) `C.finally` seal)
    )

-- | connect a committer and emitter action via spawning a queue, and wait for both to complete.
withQ ::
  (MonadConc m) =>
  Queue a ->
  (Queue a -> m (Box m a a, m ())) ->
  (Committer m a -> m l) ->
  (Emitter m a -> m r) ->
  m (l,r)
withQ q spawner cio eio =
  C.bracket
    (spawner q)
    snd
    ( \(box, seal) ->
        concurrently
          (cio (committer box) `C.finally` seal)
          (eio (emitter box) `C.finally` seal)
    )

-- | Create an unbounded queue, returning the result from the Committer action.
--
-- >>> queueL New (\c -> glue c <$|> qList [1..3]) toListM
queueL ::
  (MonadConc m) =>
  Queue a ->
  (Committer m a -> m l) ->
  (Emitter m a -> m r) ->
  m l
queueL q cm em = withQL q toBoxM cm em

-- | Create an unbounded queue, returning the result from the Emitter action.
--
-- >>> queueR New (\c -> glue c <$|> qList [1..3]) toListM
-- [3]
queueR ::
  (MonadConc m) =>
  Queue a ->
  (Committer m a -> m l) ->
  (Emitter m a -> m r) ->
  m r
queueR q cm em = withQR q toBoxM cm em

-- | Create an unbounded queue, returning both results.
--
-- >>> queue Unbounded (\c -> glue c <$|> qList [1..3]) toListM
-- ((),[1,2,3])
queue ::
  (MonadConc m) =>
  Queue a ->
  (Committer m a -> m l) ->
  (Emitter m a -> m r) ->
  m (l, r)
queue q cm em = withQ q toBoxM cm em

-- | lift a box from STM
liftB :: (MonadConc m) => Box (STM m) a b -> Box m a b
liftB (Box c e) = Box (foist atomically c) (foist atomically e)

-- | Turn a box action into a box continuation
fromAction :: (MonadConc m) => (Box m a b -> m r) -> CoBox m b a
fromAction baction = Codensity $ fuseActions baction

-- | Connect up two box actions via two queues
fuseActions :: (MonadConc m) => (Box m a b -> m r) -> (Box m b a -> m r') -> m r'
fuseActions abm bam = do
  (Box ca ea, _) <- toBoxM Unbounded
  (Box cb eb, _) <- toBoxM Unbounded
  concurrentlyRight (abm (Box ca eb)) (bam (Box cb ea))

-- | Hook a committer action to a queue, creating an emitter continuation.
emitQ :: (MonadConc m) => Queue a -> (Committer m a -> m r) -> CoEmitter m a
emitQ q cio = Codensity $ \eio -> queueR q cio eio

-- | Hook a committer action to a queue, creating an emitter continuation.
commitQ :: (MonadConc m) => Queue a -> (Emitter m a -> m r) -> CoCommitter m a
commitQ q eio = Codensity $ \cio -> queueL q cio eio
