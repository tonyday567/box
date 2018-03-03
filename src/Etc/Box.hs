{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | buffers
-- Modified based on [pipes-concurrency](https://hackage.haskell.org/package/pipes-concurrency)
--
-- https://www.schoolofhaskell.com/school/advanced-haskell/beautiful-concurrency/3-software-transactional-memory
--
module Etc.Box
  ( Box(..)
  , liftB
  , safeIOToSTM
  , waitCancel
  , withQ
  , Queue(..)
  , queue
  , queueC
  , queueE
  , queueIO
  , queueCIO
  , queueEIO
  , ends
  , commitPlug
  , emitPlug
  , boxPlug
  , boxForgetPlug
  ) where

import Control.Category
import qualified Control.Concurrent.STM as S
import qualified Control.Exception
import Control.Lens hiding ((:>), (.>), (<|), (|>))
import Data.Semigroup hiding (First, getFirst)
import Etc.Committer
import Etc.Cont
import Etc.Emitter
import GHC.Conc
import Protolude hiding ((.), (<>))

-- | 'Queue' specifies how messages are queued
data Queue a
  = Unbounded
  | Bounded Int
  | Single
  | Latest a
  | Newest Int
  | New

-- | A Box is a product of a Committer m and an Emitter. Think of a box with an incoming wire and an outgoing wire. Now notice that the abstraction is reversable: are you looking at two wires from "inside a box"; a blind erlang grunt communicating with the outside world via the two thin wires, or are you looking from "outside the box"; interacting with a black box object. Either way, it's a box.
-- And either way, the committer is contravariant and the emitter covariant so it forms a profunctor.
--
-- a Box can also be seen as having an input tape and output tape, thus available for turing and finite-state machine metaphorics.
--
data Box m c e = Box
  { committer :: Committer m c
  , emitter :: Emitter m e
  }

instance (Functor m) => Profunctor (Box m) where
  dimap f g (Box c e) = Box (contramap f c) (fmap g e)

instance (Alternative m, Monad m) => Semigroup (Box m c e) where
  (<>) (Box c e) (Box c' e') = Box (c <> c') (e <> e')

instance (Alternative m, Monad m) => Monoid (Box m c e) where
  mempty = Box mempty mempty
  mappend = (<>)

-- | lift a box from STM to IO
liftB :: Box STM a b -> Box IO a b
liftB (Box c e) = Box (liftC c) (liftE e)

-- * plugs
-- | hook an emitter action to a queue, creating a committer continuation
commitPlug :: (Emitter STM a -> IO ()) -> Cont IO (Committer STM a)
commitPlug eio = Cont $ \cio -> queueC cio eio

-- | hook a committer action to a queue, creating an emitter continuation
emitPlug :: (Committer STM a -> IO r) -> Cont IO (Emitter STM a)
emitPlug cio = Cont $ \eio -> queueE cio eio

-- | create a double-queued box plug
boxPlug ::
     (Emitter STM a -> IO ())
  -> (Committer STM b -> IO ())
  -> Cont IO (Box STM a b)
boxPlug eio cio = Box <$> commitPlug eio <*> emitPlug cio

-- | create a box plug from a box action.  Caution: implicitly, this (has to) forget interactions between emitter and committer in the one action (and it does so silently).  These forgotten interactions are typically those that create races
boxForgetPlug :: (Box STM b a -> IO ()) -> Cont IO (Box STM a b)
boxForgetPlug bio = boxPlug (bio . Box mempty) (bio . (`Box` mempty))

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
spawn :: Queue a -> IO (Box STM a a, STM ())
spawn buffer = do
  (write, read) <-
    case buffer of
      Bounded n -> do
        q <- S.newTBQueueIO n
        return (S.writeTBQueue q, S.readTBQueue q)
      Unbounded -> do
        q <- S.newTQueueIO
        return (S.writeTQueue q, S.readTQueue q)
      Single -> do
        m <- S.newEmptyTMVarIO
        return (S.putTMVar m, S.takeTMVar m)
      Latest a -> do
        t <- S.newTVarIO a
        return (S.writeTVar t, S.readTVar t)
      New -> do
        m <- S.newEmptyTMVarIO
        return (\x -> S.tryTakeTMVar m *> S.putTMVar m x, S.takeTMVar m)
      Newest n -> do
        q <- S.newTBQueueIO n
        let write x = S.writeTBQueue q x <|> (S.tryReadTBQueue q *> write x)
        return (write, S.readTBQueue q)
  sealed <- S.newTVarIO False
  let seal = S.writeTVar sealed True
    {- Use weak TVars to keep track of whether the 'Input' or 'Output' has been
       garbage collected.  Seal the mailbox when either of them becomes garbage
       collected.
    -}
  rSend <- S.newTVarIO ()
  void $ S.mkWeakTVar rSend (S.atomically seal)
  rRecv <- S.newTVarIO ()
  void $ S.mkWeakTVar rRecv (S.atomically seal)
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

spawnIO :: Queue a -> IO (Box IO a a, STM ())
spawnIO buffer = fmap (\(a, b) -> (liftB a, b)) (spawn buffer)

safeIOToSTM :: IO a -> STM a
safeIOToSTM req =
  unsafeIOToSTM $ do
    tv <- newEmptyMVar
    _ <-
      forkIO $
      ((putMVar tv . Right) =<< req) `Control.Exception.catch`
      (\(e :: SomeException) -> putMVar tv $ Left e)
    r <- takeMVar tv
    case r of
      Right x -> return x
      Left e -> Control.Exception.throw e

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
  -> (Queue a -> IO (Box m a a, STM ()))
  -> (Committer m a -> IO l)
  -> (Emitter m a -> IO r)
  -> IO (l, r)
withQ q spawner cio eio =
  bracket
    (spawner q)
    (\(_, seal) -> atomically seal)
    (\(box, seal) ->
       concurrently
         (cio (committer box) `Control.Exception.finally` atomically seal)
         (eio (emitter box) `Control.Exception.finally` atomically seal))

queue :: (Committer STM a -> IO l) -> (Emitter STM a -> IO r) -> IO (l, r)
queue = withQ Unbounded spawn

queueE :: (Committer STM a -> IO l) -> (Emitter STM a -> IO r) -> IO r
queueE cio eio = snd <$> withQ Unbounded spawn cio eio

queueC :: (Committer STM a -> IO l) -> (Emitter STM a -> IO r) -> IO l
queueC cio eio = fst <$> withQ Unbounded spawn cio eio

queueIO :: (Committer IO a -> IO l) -> (Emitter IO a -> IO r) -> IO (l, r)
queueIO = withQ Unbounded spawnIO

queueEIO :: (Committer IO a -> IO l) -> (Emitter IO a -> IO r) -> IO r
queueEIO cio eio = snd <$> withQ Unbounded spawnIO cio eio

queueCIO :: (Committer IO a -> IO l) -> (Emitter IO a -> IO r) -> IO l
queueCIO cio eio = fst <$> withQ Unbounded spawnIO cio eio

