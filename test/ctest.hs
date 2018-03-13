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
-- Roughly follows [pipes-concurrency](https://hackage.haskell.org/package/pipes-concurrency)
--
module Main where

import Etc.Box
import Etc.Committer
import Etc.Emitter
import Prelude
import Control.Applicative
import Control.Concurrent.Classy.STM as C
import Control.Monad.Conc.Class as C hiding (spawn)
import Control.Concurrent.Classy.Async as C
import Control.Monad.Catch as C
import Streaming (Of(..), Stream)
import qualified Control.Foldl as L
import qualified Streaming.Prelude as S
import Etc.Cont
import Control.Monad
import Control.Concurrent.Classy.CRef
import Data.Text
import Test.DejaFu

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

-- | connect a committer and emitter action via a queue, and wait for both to complete.
withQ :: (MonadConc m) =>
     Queue a
  -> (Queue a -> (STM m) (Box (STM m) a a, (STM m) ()))
  -> (Committer (STM m) a -> m l)
  -> (Emitter (STM m) a -> m r)
  -> m (l, r)
withQ q spawner cio eio =
  bracket
    (atomically $ spawner q)
    (\(_, seal) -> atomically seal)
    (\(box, seal) ->
       concurrently
         (cio (committer box) `finally` atomically seal)
         (eio (emitter box) `finally` atomically seal))

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

ex1 :: (MonadConc m) => m [Text]
ex1 = do
  ref <- newCRef []
  let c = fmap liftC' $ cCRef ref
  let e = fmap liftE' $ toEmit (S.take 3 $ S.map (pack . show) $ S.each [0..])
  fuse (pure . pure) $ Box <$> c <*> e
  Prelude.reverse <$> readCRef ref

ex2 :: (MonadConc m) => m [Text]
ex2 = do
  ref <- newCRef []
  let c = cCRef ref
  let e = toEmit (S.take 3 $ S.map (pack . show) $ S.each [0..])
  fuse' (pure . pure) $ Box <$> c <*> e
  Prelude.reverse <$> readCRef ref

toCommit :: (MonadConc m) => (Stream (Of a) m () -> m r) -> Cont m (Committer (STM m) a)
toCommit f =
  Cont (\c -> queueC c (\(Emitter o) -> f . toStreamM . Emitter $ o))

-- | create a committer from a fold
toCommitFold :: (MonadConc m) => L.FoldM m a () -> Cont m (Committer (STM m) a)
toCommitFold f = toCommit (fmap S.snd' . L.impurely S.foldM f)

-- | commit to a list IORef
cCRef :: (MonadConc m) => CRef m [b] -> Cont m (Committer (STM m) b)
cCRef ref =
  toCommitFold $
  L.FoldM (\x a -> modifyCRef x (a :) >> pure x) (pure ref) (const $ pure ())

-- | create an emitter from a stream
toEmit :: (MonadConc m) => Stream (Of a) m () -> Cont m (Emitter (STM m) a)
toEmit s = Cont (queueE (fromStreamM s))

liftC' :: (MonadConc m) => Committer (STM m) a -> Committer m a
liftC' c = Committer $ atomically . commit c

liftE' :: (MonadConc m) => Emitter (STM m) a -> Emitter m a
liftE' = Emitter . atomically . emit

-- | turn an emitter into a stream
toStreamM :: (MonadConc m) => Emitter (STM m) a -> Stream (Of a) m ()
toStreamM e = S.untilRight getNext
  where
    getNext = maybe (Right ()) Left <$> emit (liftE' e)

-- | turn a stream into a committer
fromStreamM :: (MonadConc m) => Stream (Of b) m () -> Committer (STM m) b -> m ()
fromStreamM s c = go s
  where
    go str = do
      eNxt <- S.next str -- uncons requires r ~ ()
      forM_ eNxt $ \(a, str') -> do
        continue <- commit (liftC' c) a
        when continue (go str')

-- * primitives
-- | fuse an emitter directly to a committer
fuse_ :: (Monad m) => Emitter m a -> Committer m a -> m ()
fuse_ e c = go
  where
    go = do
      a <- emit e
      c' <- maybe (pure False) (commit c) a
      when c' go

-- | slightly more efficient version
fuseSTM_ :: (MonadConc m) => Emitter (STM m) a -> Committer (STM m) a -> m ()
fuseSTM_ e c = go
  where
    go = do
      b <-
        atomically $ do
          a <- emit e
          maybe (pure False) (commit c) a
      when b go

fuse :: (Monad m) => (a -> m (Maybe b)) -> Cont m (Box m b a) -> m ()
fuse f box = with box $ \(Box c e) -> fuse_ (emap f e) c

fuse' :: (MonadConc m) => (a -> (STM m) (Maybe b)) -> Cont m (Box (STM m) b a) -> m ()
fuse' f box = with box $ \(Box c e) -> fuseSTM_ (emap f e) c

main :: IO ()
main = sequence_ $ autocheck <$> [ex1, ex2]
