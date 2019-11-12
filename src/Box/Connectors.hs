{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

-- | various ways to connect things up
module Box.Connectors
  ( fuse_
  , fuseSTM_
  , fuse
  , fuseSTM
  , forkEmit
  , feedback
  , feedbackE
  , fuseEmit
  , fuseEmitM
  , fuseCommit
  , fuseCommitM
  , emerge
  , emergeM
  , splitCommit
  , splitCommitSTM
  , contCommit
  ) where

import Box.Box
import Box.Queue
import Box.Committer
import Box.Cont
import Box.Emitter
import Control.Monad.Conc.Class as C
import Control.Concurrent.Classy.Async as C
import Control.Monad

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
        C.atomically $ do
          a <- emit e
          maybe (pure False) (commit c) a
      when b go

-- | fuse a box
--
-- > (fuse (pure . Just) $ liftB <$> (Box <$> cStdout 2 <*> emitter')) >> sleep 1
-- hi
-- bye
--
-- > etc () (Transducer id) == fuse (pure . pure) . fmap liftB
--
fuse :: (Monad m) => (a -> m (Maybe b)) -> Cont m (Box m b a) -> m ()
fuse f box = with box $ \(Box c e) -> fuse_ (emap f e) c

-- | fuse a box with an STM mapMaybe action
fuseSTM :: (MonadConc m) => (a -> (STM m) (Maybe b)) -> Cont m (Box (STM m) b a) -> m ()
fuseSTM f box = with box $ \(Box c e) -> fuseSTM_ (emap f e) c

-- | fuse-branch an emitter
forkEmit :: (Monad m) => Emitter m a -> Committer m a -> Emitter m a
forkEmit e c =
  Emitter $ do
    a <- emit e
    maybe (pure ()) (void <$> commit c) a
    pure a

-- * buffer hookups
-- | fuse a committer to a buffer
fuseCommit :: (MonadConc m) => Committer (STM m) a -> Cont m (Committer (STM m) a)
fuseCommit c = Cont $ \caction -> queueC caction (`fuseSTM_` c)

-- | fuse a committer to a buffer
fuseCommitM :: (MonadConc m) => Committer m a -> Cont m (Committer m a)
fuseCommitM c = Cont $ \caction -> queueCM caction (`fuse_` c)

-- | fuse an emitter to a buffer
fuseEmit :: (MonadConc m) => Emitter (STM m) a -> Cont m (Emitter (STM m) a)
fuseEmit e = Cont $ \eaction -> queueE (fuseSTM_ e) eaction

-- | fuse an emitter to a buffer
fuseEmitM :: (MonadConc m) => Emitter m a -> Cont m (Emitter m a)
fuseEmitM e = Cont $ \eaction -> queueEM (fuse_ e) eaction


-- | merge two emitters
--
-- This differs from `liftA2 (<>)` in that the monoidal (and alternative) instance of an Emitter is left-biased (The left emitter exhausts before the right one is begun). This merge is concurrent.
--
emerge ::
  (MonadConc m) =>
  Cont m (Emitter (STM m) a, Emitter (STM m) a) ->
  Cont m (Emitter (STM m) a)
emerge e =
  Cont $ \eaction ->
    with e $ \e' ->
      fst <$>
      C.concurrently
        (queueE (fuseSTM_ (fst e')) eaction)
        (queueE (fuseSTM_ (snd e')) eaction)

-- | monadic version
--
emergeM ::
  (MonadConc m) =>
  Cont m (Emitter m a, Emitter m a) ->
  Cont m (Emitter m a)
emergeM e =
  Cont $ \eaction ->
    with e $ \e' ->
      fst <$>
      C.concurrently
        (queueEM (fuse_ (fst e')) eaction)
        (queueEM (fuse_ (snd e')) eaction)

-- | split a committer (STM m)
--
splitCommitSTM :: (MonadConc m) =>
     Cont m (Committer (STM m) a)
  -> Cont m (Either (Committer (STM m) a) (Committer (STM m) a))
splitCommitSTM c =
  Cont $ \kk ->
    with c $ \c' ->
      concurrentlyLeft
        (queueC (kk . Left) (`fuseSTM_` c'))
        (queueC (kk . Right) (`fuseSTM_` c'))

-- | split a committer
--
splitCommit :: (MonadConc m) =>
     Cont m (Committer m a)
  -> Cont m (Either (Committer m a) (Committer m a))
splitCommit c =
  Cont $ \kk ->
    with c $ \c' ->
      concurrentlyLeft
        (queueCM (kk . Left) (`fuse_` c'))
        (queueCM (kk . Right) (`fuse_` c'))

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
      fuse_ (emap f e) c
      bio (Box c e)

-- | an emitter post-processor that cons transformed emissions back into the emitter
feedbackE ::
  (MonadConc m) =>
  (a -> m (Maybe a)) ->
  Emitter m a ->
  Cont m (Emitter m a)
feedbackE f e =
  emergeM ((,) <$> pure e <*> fuseEmitM (emap f e))
