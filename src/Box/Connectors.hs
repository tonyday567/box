{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

-- | various ways to connect things up
module Box.Connectors
  ( fromListE,
    fromList_,
    toList_,
    emitQ,
    commitQ,
    sink,
    source,
    forkEmit,
    feedback,
    feedbackE,
    queueCommitter,
    queueEmitter,
    emerge,
    splitCommit,
    contCommit,
  )
where

import Box.Box
import Box.Committer
import Box.Cont
import Box.Emitter
import Box.Queue
import Control.Concurrent.Classy.Async as C
import Control.Monad.Conc.Class (MonadConc)
import NumHask.Prelude hiding (STM, atomically)

-- * primitives

-- | Turn a list into an 'Emitter' continuation via a 'Queue'
fromListE :: (MonadConc m) => [a] -> Cont m (Emitter m a)
fromListE xs = Cont $ queueE (eListC (Emitter . pure . Just <$> xs))

eListC :: (Monad m) => [Emitter m a] -> Committer m a -> m ()
eListC [] _ = pure ()
eListC (e : es) c = do
  x <- emit e
  case x of
    Nothing -> pure ()
    Just x' -> commit c x' *> eListC es c

-- | fromList_ directly supplies to a committer action
fromList_ :: Monad m => [a] -> Committer m a -> m ()
fromList_ xs c = flip evalStateT xs $ glue (hoist lift c) stateE

-- | toList_ directly receives from an emitter action
toList_ :: (Monad m) => Emitter m a -> m [a]
toList_ e = flip execStateT [] $ glue stateC (hoist lift e)

-- | hook a committer action to a queue, creating an emitter continuation
emitQ :: (MonadConc m) => (Committer m a -> m r) -> Cont m (Emitter m a)
emitQ cio = Cont $ \eio -> queueE cio eio

-- | hook a committer action to a queue, creating an emitter continuation
commitQ :: (MonadConc m) => (Emitter m a -> m r) -> Cont m (Committer m a)
commitQ eio = Cont $ \cio -> queueC cio eio

-- | singleton sink
sink1 :: (Monad m) => (a -> m ()) -> Emitter m a -> m ()
sink1 f e = do
  a <- emit e
  case a of
    Nothing -> pure ()
    Just a' -> f a'

-- | finite sink
sink :: (MonadConc m) => (a -> m ()) -> Int -> Cont m (Committer m a)
sink f n = commitQ $ replicateM_ n . (sink1 f)

-- | singleton source
source1 :: (Monad m) => m a -> Committer m a -> m ()
source1 a c = do
  a' <- a
  void $ commit c a'

-- | finite source
source :: (MonadConc m) => m a -> Int -> Cont m (Emitter m a)
source f n = emitQ $ replicateM_ n . (source1 f)

-- | fork branch an emitter
forkEmit :: (Monad m) => Emitter m a -> Committer m a -> Emitter m a
forkEmit e c =
  Emitter $ do
    a <- emit e
    maybe (pure ()) (void <$> commit c) a
    pure a

-- * buffer hookups

-- | fuse a committer to a buffer
queueCommitter :: (MonadConc m) => Committer m a -> Cont m (Committer m a)
queueCommitter c = Cont $ \caction -> queueC caction (glue c)

-- | fuse an emitter to a buffer
queueEmitter :: (MonadConc m) => Emitter m a -> Cont m (Emitter m a)
queueEmitter e = Cont $ \eaction -> queueE (`glue` e) eaction

-- | merge two emitters
--
-- This differs from `liftA2 (<>)` in that the monoidal (and alternative) instance of an Emitter is left-biased (The left emitter exhausts before the right one is begun). This merge is concurrent.
emerge ::
  (MonadConc m) =>
  Cont m (Emitter m a, Emitter m a) ->
  Cont m (Emitter m a)
emerge e =
  Cont $ \eaction ->
    with e $ \e' ->
      fst
        <$> C.concurrently
          (queueE (`glue` (fst e')) eaction)
          (queueE (`glue` (snd e')) eaction)

-- | split a committer
splitCommit ::
  (MonadConc m) =>
  Cont m (Committer m a) ->
  Cont m (Either (Committer m a) (Committer m a))
splitCommit c =
  Cont $ \kk ->
    with c $ \c' ->
      concurrentlyLeft
        (queueC (kk . Left) (glue c'))
        (queueC (kk . Right) (glue c'))

-- | use a split committer
contCommit :: Either (Committer m a) (Committer m b) -> (Committer m a -> Committer m b) -> Committer m b
contCommit ec f =
  Committer $ \a ->
    case ec of
      Left lc -> commit (f lc) a
      Right rc -> commit rc a

-- | a box modifier that feeds commits back to the emitter
feedback ::
  (MonadConc m) =>
  (a -> m (Maybe b)) ->
  Cont m (Box m b a) ->
  Cont m (Box m b a)
feedback f box =
  Cont $ \bio ->
    with box $ \(Box c e) -> do
      glue c (emap f e)
      bio (Box c e)

-- | an emitter post-processor that cons transformed emissions back into the emitter
feedbackE ::
  (MonadConc m) =>
  (a -> m (Maybe a)) ->
  Emitter m a ->
  Cont m (Emitter m a)
feedbackE f e =
  emerge ((,) <$> pure e <*> queueEmitter (emap f e))
