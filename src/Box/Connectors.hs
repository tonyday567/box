{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | various ways to connect things up
module Box.Connectors
  ( fuse_
  , fuseSTM_
  , fuse
  , forkEmit
  , feedback
  , feedbackE
  , fuseEmit
  , fuseCommit
  , emerge
  , splitCommit
  , contCommit
  ) where

import Control.Category
import Control.Lens hiding ((:>), (.>), (<|), (|>))
import Data.Semigroup hiding (First, getFirst)
import Box.Box
import Box.Queue
import Box.Committer
import Box.Cont
import Box.Emitter
import Protolude hiding ((.), (<>))

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
fuseSTM_ :: Emitter STM a -> Committer STM a -> IO ()
fuseSTM_ e c = go
  where
    go = do
      b <-
        atomically $ do
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

-- | fuse-branch an emitter
forkEmit :: (Monad m) => Emitter m a -> Committer m a -> Emitter m a
forkEmit e c =
  Emitter $ do
    a <- emit e
    maybe (pure ()) (void <$> commit c) a
    pure a

-- * buffer hookups
-- | fuse a committer to a buffer
fuseCommit :: Committer STM a -> Cont IO (Committer STM a)
fuseCommit c = Cont $ \cio -> queueC cio (`fuseSTM_` c)

-- | fuse an emitter to a buffer, with a transformation
fuseEmit :: Emitter STM a -> Cont IO (Emitter STM a)
fuseEmit e = Cont $ \eio -> queueE (fuseSTM_ e) eio

-- | merge two emitters
--
-- This differs from `liftA2 (<>)` in that the monoidal (and alternative) instance of an Emitter is left-biased (The left emitter exhausts before the right one is begun). This merge is concurrent.
--
emerge :: Cont IO (Emitter STM a, Emitter STM a) -> Cont IO (Emitter STM a)
emerge e =
  Cont $ \eio ->
    with e $ \e' ->
      fst <$>
      concurrently
        (queueE (fuseSTM_ (fst e')) eio)
        (queueE (fuseSTM_ (snd e')) eio)

-- | merge two committers
--
-- not working
--
splitCommit ::
     Cont IO (Committer IO a)
  -> Cont IO (Either (Committer IO a) (Committer IO a))
splitCommit c =
  Cont $ \kk ->
    with c $ \c' ->
      fst <$>
      concurrently
        (queueCIO (kk . Left) (`fuse_` c'))
        (queueCIO (kk . Right) (`fuse_` c'))

-- | a failed attempt to understand the either continuation style
contCommit :: Either (Committer m Text) (Committer m Text) -> Committer m Text
contCommit ec =
  Committer $ \a ->
    case ec of
      Left lc -> commit (contramap ("left " <>) lc) a
      Right rc -> commit rc a

-- | a box modifier that feeds commits back to the emitter
feedback :: (a -> IO (Maybe b)) -> Cont IO (Box IO b a) -> Cont IO (Box IO b a)
feedback f box =
  Cont $ \bio ->
    with box $ \(Box c e) -> do
      fuse_ (emap f e) c
      bio (Box c e)

-- | an emitter post-processor that cons transformed emissions back into the emitter
feedbackE :: (a -> IO (Maybe a)) -> Emitter STM a -> Cont IO (Emitter STM a)
feedbackE f e =
  emerge ((,) <$> pure e <*> fuseEmit (emap (safeIOToSTM . f) e))
