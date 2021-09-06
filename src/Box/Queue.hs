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
    fromAction,
    fuseActions,
  )
where

import Box.Box
import Box.Committer
import Box.Cont
import Box.Emitter
import Control.Concurrent.Async as C
import Control.Concurrent.STM as C
import Control.Monad.Catch as C
import Prelude
import Control.Monad.Morph
import Control.Applicative

-- | 'Queue' specifies how messages are queued
data Queue a
  = Unbounded
  | Bounded Int
  | Single
  | Latest a
  | Newest Int
  | New

-- | create a queue, returning the ends
ends :: Queue a -> STM (a -> STM (), STM a)
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
writeCheck :: TVar Bool -> (a -> STM ()) -> a -> STM Bool
writeCheck sealed i a = do
  b <- readTVar sealed
  if b
    then pure False
    else do
      i a
      pure True

-- | read from a queue, and retry if not sealed
readCheck :: TVar Bool -> STM a -> STM (Maybe a)
readCheck sealed o =
  (Just <$> o)
    <|> ( do
            b <- readTVar sealed
            C.check b
            pure Nothing
        )

-- | turn a queue into a box (and a seal)
toBox ::
  Queue a ->
  STM (Box STM a a, STM ())
toBox q = do
  (i, o) <- ends q
  sealed <- newTVar False
  let seal = writeTVar sealed True
  pure
    ( Box
        (Committer (writeCheck sealed i))
        (Emitter (readCheck sealed o)),
      seal
    )

-- | turn a queue into a box (and a seal), and lift from STM to the underlying monad.
toBoxM ::
  Queue a ->
  IO (Box IO a a, IO ())
toBoxM q = do
  (b, s) <- atomically $ toBox q
  pure (liftB b, atomically s)

-- | wait for the first action, and then cancel the second
waitCancel :: IO b -> IO a -> IO b
waitCancel a b =
  C.withAsync a $ \a' ->
    C.withAsync b $ \b' -> do
      a'' <- C.wait a'
      C.cancel b'
      pure a''

-- | run two actions concurrently, but wait and return on the left result.
concurrentlyLeft :: IO a -> IO b -> IO a
concurrentlyLeft left right =
  C.withAsync left $ \a ->
    C.withAsync right $ \_ ->
      C.wait a

-- | run two actions concurrently, but wait and return on the right result.
concurrentlyRight :: IO a -> IO b -> IO b
concurrentlyRight left right =
  C.withAsync left $ \_ ->
    C.withAsync right $ \b ->
      C.wait b

-- | connect a committer and emitter action via spawning a queue, and wait for both to complete.
withQC ::
  Queue a ->
  (Queue a -> IO (Box IO a a, IO ())) ->
  (Committer IO a -> IO l) ->
  (Emitter IO a -> IO r) ->
  IO l
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
  Queue a ->
  (Queue a -> IO (Box IO a a, IO ())) ->
  (Committer IO a -> IO l) ->
  (Emitter IO a -> IO r) ->
  IO r
withQE q spawner cio eio =
  bracket
    (spawner q)
    snd
    ( \(box, seal) ->
        concurrentlyRight
          (cio (committer box) `C.finally` seal)
          (eio (emitter box) `C.finally` seal)
    )

-- | create an unbounded queue, returning the emitter result
queueC ::
  (Committer IO a -> IO l) ->
  (Emitter IO a -> IO r) ->
  IO l
queueC cm em = withQC Unbounded toBoxM cm em

-- | create an unbounded queue, returning the emitter result
queueE ::
  (Committer IO a -> IO l) ->
  (Emitter IO a -> IO r) ->
  IO r
queueE cm em = withQE Unbounded toBoxM cm em

-- | lift a box from STM
liftB :: Box STM a b -> Box IO a b
liftB (Box c e) = Box (hoist atomically c) (hoist atomically e)

-- | turn a box action into a box continuation
fromAction :: (Box IO a b -> IO r) -> Cont IO (Box IO b a)
fromAction baction = Cont $ fuseActions baction

-- | connect up two box actions via two queues
fuseActions :: (Box IO a b -> IO r) -> (Box IO b a -> IO r') -> IO r'
fuseActions abm bam = do
  (Box ca ea, _) <- toBoxM Unbounded
  (Box cb eb, _) <- toBoxM Unbounded
  concurrentlyRight (abm (Box ca eb)) (bam (Box cb ea))
