{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

-- | various ways to connect things up
module Box.Connectors
  ( fromListE,
    fromList_,
    toList_,
    fromToList_,
    emitQ,
    commitQ,
    sink,
    source,
    forkEmit,
    feedback,
    queueCommitter,
    queueEmitter,
    concurrentE,
    concurrentC,
    glueN,
  )
where

import Box.Box
import Box.Committer
import Box.Cont
import Box.Emitter
import Box.Queue
import Control.Concurrent.Async as C
import Control.Lens
import qualified Data.Sequence as Seq
import Prelude
import Control.Monad.Morph
import Control.Monad.Trans.State.Lazy
import Control.Monad
import Data.Foldable

-- | Turn a list into an 'Emitter' continuation via a 'Queue'
fromListE :: [a] -> Cont IO (Emitter IO a)
fromListE xs = Cont $ queueE (eListC (Emitter . pure . Just <$> xs))

eListC :: (Monad m) => [Emitter m a] -> Committer m a -> m ()
eListC [] _ = pure ()
eListC (e : es) c = do
  x <- emit e
  case x of
    Nothing -> pure ()
    Just x' -> commit c x' *> eListC es c

-- | fromList_ directly supplies to a committer action
--
-- FIXME: fromList_ combined with cRef is failing dejavu concurrency testing...
fromList_ :: Monad m => [a] -> Committer m a -> m ()
fromList_ xs c = flip evalStateT (Seq.fromList xs) $ glue (hoist lift c) stateE

-- | toList_ directly receives from an emitter
--
-- TODO: check isomorphism
--
-- > toList_ == toListE
toList_ :: (Monad m) => Emitter m a -> m [a]
toList_ e = toList <$> flip execStateT Seq.empty (glue stateC (hoist lift e))

-- | Glues a committer and emitter, taking n emits
--
-- >>> glueN 4 <$> pure (contramap show toStdout) <*.> fromListE [1..]
-- 1
-- 2
-- 3
-- 4
glueN :: Monad m => Int -> Committer m a -> Emitter m a -> m ()
glueN n c e = flip evalStateT 0 $ glue (hoist lift c) (takeE n e)

-- | take a list, emit it through a box, and output the committed result.
--
-- The pure nature of this computation is highly useful for testing,
-- especially where parts of the box under investigation has non-deterministic attributes.
fromToList_ :: (Monad m) => [a] -> (Box (StateT (Seq.Seq b, Seq.Seq a) m) b a -> StateT (Seq.Seq b, Seq.Seq a) m r) -> m [b]
fromToList_ xs f = do
  (res, _) <-
    flip execStateT (Seq.empty, Seq.fromList xs) $
      f (Box (hoist (zoom _1) stateC) (hoist (zoom _2) stateE))
  pure $ toList res

-- | hook a committer action to a queue, creating an emitter continuation
emitQ :: (Committer IO a -> IO r) -> Cont IO (Emitter IO a)
emitQ cio = Cont $ \eio -> queueE cio eio

-- | hook a committer action to a queue, creating an emitter continuation
commitQ :: (Emitter IO a -> IO r) -> Cont IO (Committer IO a)
commitQ eio = Cont $ \cio -> queueC cio eio

-- | singleton sink
sink1 :: (Monad m) => (a -> m ()) -> Emitter m a -> m ()
sink1 f e = do
  a <- emit e
  case a of
    Nothing -> pure ()
    Just a' -> f a'

-- | finite sink
sink :: Int -> (a -> IO ()) -> Cont IO (Committer IO a)
sink n f = commitQ $ replicateM_ n . sink1 f

-- | singleton source
source1 :: (Monad m) => m a -> Committer m a -> m ()
source1 a c = do
  a' <- a
  void $ commit c a'

-- | finite source
source :: Int -> IO a -> Cont IO (Emitter IO a)
source n f = emitQ $ replicateM_ n . source1 f

-- | glues an emitter to a committer, then resupplies the emitter
forkEmit :: (Monad m) => Emitter m a -> Committer m a -> Emitter m a
forkEmit e c =
  Emitter $ do
    a <- emit e
    maybe (pure ()) (void <$> commit c) a
    pure a

-- | fuse a committer to a buffer
queueCommitter :: Committer IO a -> Cont IO (Committer IO a)
queueCommitter c = Cont $ \caction -> queueC caction (glue c)

-- | fuse an emitter to a buffer
queueEmitter :: Emitter IO a -> Cont IO (Emitter IO a)
queueEmitter e = Cont $ \eaction -> queueE (`glue` e) eaction

-- | concurrently run two emitters
--
-- This differs from mappend in that the monoidal (and alternative) instance of an Emitter is left-biased (The left emitter exhausts before the right one is begun). This is non-deterministically concurrent.
concurrentE ::
  Emitter IO a ->
  Emitter IO a ->
  Cont IO (Emitter IO a)
concurrentE e e' =
  Cont $ \eaction ->
    fst
      <$> C.concurrently
        (queueE (`glue` e) eaction)
        (queueE (`glue` e') eaction)

-- | run two committers concurrently
concurrentC :: Committer IO a -> Committer IO a -> Cont IO (Committer IO a)
concurrentC c c' = mergeC <$> eitherC c c'

eitherC ::
  Committer IO a ->
  Committer IO a ->
  Cont IO (Either (Committer IO a) (Committer IO a))
eitherC cl cr =
  Cont $
    \kk ->
      fst
        <$> C.concurrently
          (queueC (kk . Left) (glue cl))
          (queueC (kk . Right) (glue cr))

mergeC :: Either (Committer IO a) (Committer IO a) -> Committer IO a
mergeC ec =
  Committer $ \a ->
    case ec of
      Left lc -> commit lc a
      Right rc -> commit rc a

-- | a box modifier that feeds commits back to the emitter
feedback ::
  (Monad m) =>
  (a -> m (Maybe b)) ->
  Cont m (Box m b a) ->
  Cont m (Box m b a)
feedback f box =
  Cont $ \bio ->
    with box $ \(Box c e) -> do
      glue c (mapE f e)
      bio (Box c e)
