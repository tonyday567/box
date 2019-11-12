{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  , waitCancel
  , ends
  , withQ
  , withQE
  , withQC
  , toBox
  , concurrentlyLeft
  , concurrentlyRight
  ) where

import Box.Box
import Box.Committer
import Box.Emitter
import Control.Concurrent.Classy.STM as C
import Control.Monad.Conc.Class as C
import Control.Concurrent.Classy.Async as C
import Control.Monad.Catch as C
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

-- | wait for the first action, and then cancel the second
waitCancel :: (MonadConc m) => m b -> m a -> m b
waitCancel a b =
  withAsync a $ \a' ->
    withAsync b $ \b' -> do
      a'' <- wait a'
      cancel b'
      pure a''

-- | run two actions concurrently, but wait and return on the left result.
concurrentlyLeft :: MonadConc m => m a -> m b -> m a
concurrentlyLeft left right =
  withAsync left $ \a ->
  withAsync right $ \_ ->
  wait a

-- | run two actions concurrently, but wait and return on the right result.
concurrentlyRight :: MonadConc m => m a -> m b -> m b
concurrentlyRight left right =
  withAsync left $ \_ ->
  withAsync right $ \b ->
  wait b

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

-- | connect a committer and emitter action via spawning a queue, and wait for committer to complete.
withQC :: (MonadConc m) =>
     Queue a
  -> (Queue a -> (STM m) (Box (STM m) a a, (STM m) ()))
  -> (Committer (STM m) a -> m l)
  -> (Emitter (STM m) a -> m r)
  -> m l
withQC q spawner cio eio =
  C.bracket
    (atomically $ spawner q)
    (\(_, seal) -> atomically seal)
    (\(box, seal) ->
       concurrentlyLeft
         (cio (committer box) `C.finally` atomically seal)
         (eio (emitter box) `C.finally` atomically seal))

-- | connect a committer and emitter action via spawning a queue, and wait for emitter to complete.
withQE :: (MonadConc m) =>
     Queue a
  -> (Queue a -> (STM m) (Box (STM m) a a, (STM m) ()))
  -> (Committer (STM m) a -> m l)
  -> (Emitter (STM m) a -> m r)
  -> m r
withQE q spawner cio eio =
  C.bracket
    (atomically $ spawner q)
    (\(_, seal) -> atomically seal)
    (\(box, seal) ->
       concurrentlyRight
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


{- |

The one-in-the-chamber problem

This is the referential transparency refactoring I did to solve the one-in-the-chamber problem.  An etc process wasn't closing down when it should, until the committer fired once more:

-- etc () (Transducer $ \s -> s & S.takeWhile (/="q")) (Box <$> cStdout 2 <*> eStdin 2)

On entering a 'q' in stdin, this code piece requires another input from stdin before it shuts down.

-}

-- > etc () (Transducer $ \s -> s & S.takeWhile (/="q")) (Box <$> cStdout 2 <*> eStdin 2)
-- etc substitution
-- > with (Box <$> cStdout 2 <*> eStdin 2) $ \(Box c e) -> (e & toStream & transduce (Transducer $ \s -> s & S.takeWhile (/="q")) & fromStream) c & flip execStateT ()
-- no state & transduction unwrapping
-- > with (Box <$> cStdout 2 <*> eStdin 2) $ \(Box c e) -> (e & toStream & S.takeWhile (/="q") & fromStream) c
-- subbing the IO's
-- > with (Box <$> (eStdout 2 & commitPlug) <*> (cStdin 2 & emitPlug)) $ \(Box c e) -> (e & toStream & transduce (Transducer $ \s -> s & S.takeWhile (/="q")) & fromStream) c
-- unplugging
-- > with (Box <$> (Cont $ \cio -> queueC cio (eStdout 2)) <*> (Cont $ \eio -> queueE (cStdin 2) eio)) $ \(Box c e) -> (e & toStream & S.takeWhile (/="q") & fromStream) c
-- fmapping the Box
-- > Cont (\r_ -> (Cont $ \cio -> queueC cio (eStdout 2)) `with` \x -> r_ (Box x))
-- twisting the with simplifying the Cont
-- > Cont (\r_ -> queueC (r_ . Box) (eStdout 2))
-- spaceship time!
-- > Cont (\r_ -> (Cont (\e -> queueC (e . Box) (eStdout 2))) `with` \f -> (Cont $ \eio -> queueE (cStdin 2) eio) `with` \x -> r_ (f x))
-- flipping the withs
-- > Cont (\r_ -> with (Cont (\e -> queueC (e . Box) (eStdout 2))) (\f -> with (Cont $ \eio -> queueE (cStdin 2) eio) (r_ . f)))
-- swallowing the withs
-- > with Cont (\r_ -> queueC ((\f -> queueE (cStdin 2) (r_ . f)) . Box) (eStdout 2))

-- subbing back in mainline
-- > with (Cont (\r_ -> queueC ((\f -> queueE (cStdin 2) (r_ . f)) . Box) (eStdout 2))) (\(Box c e) -> (e & toStream & S.takeWhile (/="q") & fromStream) c)
-- > queueC ((\f -> queueE (cStdin 2) ((\(Box c e) -> (e & toStream & S.takeWhile (/="q") & fromStream) c) . f)) . Box) (eStdout 2)
-- subbing queues
-- > fmap fst (withQ Unbounded toBox ((\f -> queueE (cStdin 2) ((\(Box c e) -> (e & toStream & S.takeWhile (/="q") & fromStream) c) . f)) . Box) (eStdout 2))
-- subbing stdin and stdout
-- > fmap fst (withQ Unbounded toBox ((\f -> fmap snd (withQ Unbounded toBox (\c -> cStdin_ c *> cStdin_ c) ((\(Box c e) -> (e & toStream & S.takeWhile (/="q") & fromStream) c) . f))) . Box) (\e -> eStdout_ e *> eStdout_ e))
-- removes the second eStdout_ (still requires another stdin input before it closes up)
-- fmap fst (withQ Unbounded toBox ((\f -> fmap snd (withQ Unbounded toBox (\c -> cStdin_ c *> cStdin_ c) ((\(Box c e) -> (e & toStream & S.takeWhile (/="q") & fromStream) c) . f))) . Box) eStdout_)
-- remove surperfluous fsts and snds
-- > withQ Unbounded toBox ((\f -> (withQ Unbounded toBox (\c -> cStdin_ c *> cStdin_ c) ((\(Box c e) -> (e & toStream & S.takeWhile (/="q") & fromStream) c) . f))) . Box) eStdout_
-- IO (((),()), ())
-- an intuitive unwrapping of the f
-- > withQ Unbounded toBox (\c -> (withQ Unbounded toBox (\c' -> cStdin_ c' *> cStdin_ c') ((\e -> (e & toStream & S.takeWhile (/="q") & fromStream) c)))) eStdout_

{-

And here was the problem is much easier to see. The withQ's were waiting on both sides of the queue.

I replaced `snd <$> withQ` with `withQE`

-}

-- subbing withQE fixes!
-- withQ Unbounded toBox (\c -> (withQE Unbounded toBox (\c' -> cStdin_ c' *> cStdin_ c') ((\e -> (fromStream . S.takeWhile (/="q") . toStream $ e) c)))) eStdout_


