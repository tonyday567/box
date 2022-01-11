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
    toListE',
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
import Control.Concurrent.Classy.Async as C
import Control.Lens
import Control.Monad.Conc.Class (MonadConc)
import Control.Monad.Morph
import Control.Monad.State.Lazy
import Data.Foldable
import qualified Data.Sequence as Seq
import Prelude

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XGADTs
-- >>> :set -XFlexibleContexts
-- >>> import Data.Functor.Contravariant
-- >>> import Box
-- >>> import Control.Applicative
-- >>> import Control.Monad.Conc.Class as C
-- >>> import Control.Lens
-- >>> import qualified Data.Sequence as Seq
-- >>> import Data.Text (pack, Text)
-- >>> import Data.Functor.Contravariant

-- | Turn a list into an 'Emitter' continuation via a 'Queue'
fromListE :: (MonadConc m) => [a] -> CoEmitter m a
fromListE xs = Codensity $ queueE (eListC (Emitter . pure . Just <$> xs))

eListC :: (Monad m) => [Emitter m a] -> Committer m a -> m ()
eListC [] _ = pure ()
eListC (e : es) c = do
  x <- emit e
  case x of
    Nothing -> pure ()
    Just x' -> commit c x' *> eListC es c

-- | fromList_ directly supplies to a committer action
--
fromList_ :: Monad m => [a] -> Committer m a -> m ()
fromList_ xs c = flip evalStateT (Seq.fromList xs) $ glue (hoist lift c) stateE

-- | turn an emitter into a list
--
-- uses StateT internally, but should be the same as 'toListE', which uses recursion.
--
-- > toList_ == toListE
toListE' :: (Monad m) => Emitter m a -> m [a]
toListE' e = toList <$> flip execStateT Seq.empty (glue stateC (hoist lift e))

-- | Glues a committer and emitter, taking n emits
--
-- >>> glueN 4 <$> pure (contramap (pack . show) toStdout) <*.> fromListE [1..]
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
emitQ :: (MonadConc m) => (Committer m a -> m r) -> CoEmitter m a
emitQ cio = Codensity $ \eio -> queueE cio eio

-- | hook a committer action to a queue, creating an emitter continuation
commitQ :: (MonadConc m) => (Emitter m a -> m r) -> CoCommitter m a
commitQ eio = Codensity $ \cio -> queueC cio eio

-- | singleton sink
sink1 :: (Monad m) => (a -> m ()) -> Emitter m a -> m ()
sink1 f e = do
  a <- emit e
  case a of
    Nothing -> pure ()
    Just a' -> f a'

-- | finite sink
sink :: (MonadConc m) => Int -> (a -> m ()) -> CoCommitter m a
sink n f = commitQ $ replicateM_ n . sink1 f

-- | singleton source
source1 :: (Monad m) => m a -> Committer m a -> m ()
source1 a c = do
  a' <- a
  void $ commit c a'

-- | finite source
source :: (MonadConc m) => Int -> m a -> CoEmitter m a
source n f = emitQ $ replicateM_ n . source1 f

-- | glues an emitter to a committer, then resupplies the emitter
forkEmit :: (Monad m) => Emitter m a -> Committer m a -> Emitter m a
forkEmit e c =
  Emitter $ do
    a <- emit e
    maybe (pure ()) (void <$> commit c) a
    pure a

-- | fuse a committer to a buffer
queueCommitter :: (MonadConc m) => Committer m a -> CoCommitter m a
queueCommitter c = Codensity $ \caction -> queueC caction (glue c)

-- | fuse an emitter to a buffer
queueEmitter :: (MonadConc m) => Emitter m a -> CoEmitter m a
queueEmitter e = Codensity $ \eaction -> queueE (`glue` e) eaction

-- | concurrently run two emitters
--
-- This differs from mappend in that the monoidal (and alternative) instance of an Emitter is left-biased (The left emitter exhausts before the right one is begun). This is non-deterministically concurrent.
concurrentE ::
  (MonadConc m) =>
  Emitter m a ->
  Emitter m a ->
  CoEmitter m a
concurrentE e e' =
  Codensity $ \eaction ->
    fst
      <$> C.concurrently
        (queueE (`glue` e) eaction)
        (queueE (`glue` e') eaction)

-- | run two committers concurrently
concurrentC :: (MonadConc m) => Committer m a -> Committer m a -> CoCommitter m a
concurrentC c c' = mergeC <$> eitherC c c'

eitherC ::
  (MonadConc m) =>
  Committer m a ->
  Committer m a ->
  Codensity m (Either (Committer m a) (Committer m a))
eitherC cl cr =
  Codensity $
    \kk ->
      fst
        <$> C.concurrently
          (queueC (kk . Left) (glue cl))
          (queueC (kk . Right) (glue cr))

mergeC :: Either (Committer m a) (Committer m a) -> Committer m a
mergeC ec =
  Committer $ \a ->
    case ec of
      Left lc -> commit lc a
      Right rc -> commit rc a

-- | a box modifier that feeds commits back to the emitter
feedback ::
  (MonadConc m) =>
  (a -> m (Maybe b)) ->
  CoBox m b a ->
  CoBox m b a
feedback f box =
  Codensity $ \bio ->
    runCodensity box $ \(Box c e) -> do
      glue c (mapE f e)
      bio (Box c e)
