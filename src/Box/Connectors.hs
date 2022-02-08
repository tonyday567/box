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
  ( qList,
    qList',
    popList,
    pushList,
    pushListN,
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
import Control.Monad.State.Lazy
import Data.Foldable
import qualified Data.Sequence as Seq
import Prelude
import Box.Functor

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Box
-- >>> import Prelude
-- >>> import Data.Bool
-- >>> import Control.Monad

-- | queue a list
--
-- Returns early if a commit fails.
--
-- >>> pushList <$|> qList [1,2,3]
-- [1,2,3]
--
qList :: (MonadConc m) => [a] -> CoEmitter m a
qList xs = emitQ Unbounded (\c -> fmap and (traverse (commit c) xs))

-- | strictly queue a list
--
-- queue list version that ignores the commit flag
qList' :: (MonadConc m) => [a] -> CoEmitter m a
qList' xs = emitQ Unbounded (\c -> traverse_ (commit c) xs)

-- \e -> fmap (any isNothing) (traverse emit (repeat e))

-- | directly supply a list to a committer action, via pop
--
popList :: Monad m => [a] -> Committer m a -> m ()
popList xs c = flip evalStateT (Seq.fromList xs) $ glue (foist lift c) pop

-- | push an into a list, via push
--
pushList :: (Monad m) => Emitter m a -> m [a]
pushList e = toList <$> flip execStateT Seq.empty (glue push (foist lift e))

pushListN :: (Monad m) => Int -> Emitter m a -> m [a]
pushListN n e = toList <$> flip execStateT Seq.empty (glueN n push (foist lift e))

-- | Glues a committer and emitter, taking n emits
--
-- > glueN 4 <$> pure (contramap (pack . show) toStdout) <*.> qList [1..]
-- 1
-- 2
-- 3
-- 4
glueN :: Monad m => Int -> Committer m a -> Emitter m a -> m ()
glueN n c e = flip evalStateT 0 $ glue (foist lift c) (takeE n e)

-- | take a list, emit it through a box, and output the committed result.
--
-- The pure nature of this computation is highly useful for testing,
-- especially where parts of the box under investigation has non-deterministic attributes.
fromToList_ :: (Monad m) => [a] -> (Box (StateT (Seq.Seq b, Seq.Seq a) m) b a -> StateT (Seq.Seq b, Seq.Seq a) m r) -> m [b]
fromToList_ xs f = do
  (res, _) <-
    flip execStateT (Seq.empty, Seq.fromList xs) $
      f (Box (foist (zoom _1) push) (foist (zoom _2) pop))
  pure $ toList res

-- | singleton sink
sink1 :: (Monad m) => (a -> m ()) -> Emitter m a -> m ()
sink1 f e = do
  a <- emit e
  case a of
    Nothing -> pure ()
    Just a' -> f a'

-- | finite sink
sink :: (MonadConc m) => Int -> (a -> m ()) -> CoCommitter m a
sink n f = commitQ Unbounded $ replicateM_ n . sink1 f

-- | singleton source
source1 :: (Monad m) => m a -> Committer m a -> m ()
source1 a c = do
  a' <- a
  void $ commit c a'

-- | finite source
source :: (MonadConc m) => Int -> m a -> CoEmitter m a
source n f = emitQ Unbounded $ replicateM_ n . source1 f

-- | glues an emitter to a committer, then resupplies the emitter
forkEmit :: (Monad m) => Emitter m a -> Committer m a -> Emitter m a
forkEmit e c =
  Emitter $ do
    a <- emit e
    maybe (pure ()) (void <$> commit c) a
    pure a

-- | queue a committer
queueCommitter :: (MonadConc m) => Committer m a -> CoCommitter m a
queueCommitter c = Codensity $ \caction -> queueL Unbounded caction (glue c)

-- | queue an emitter
queueEmitter :: (MonadConc m) => Emitter m a -> CoEmitter m a
queueEmitter e = Codensity $ \eaction -> queueR Unbounded (`glue` e) eaction

-- | concurrently run two emitters
--
-- This differs to (<>), which is left-biased.
--
-- Note that functions such as toListM, which complete on the first Nothing emitted, will not work as expected.
--
-- >>> close $ (fmap toListM) (join $ concurrentE Single <$> qList [1..3] <*> qList [5..9])
-- [1,2,3]
--
-- In the code below, the ordering is non-deterministic.
--
-- > (c,l) <- cRef :: IO (Committer IO Int, IO [Int])
-- > close $ glue c <$> (join $ concurrentE Single <$> qList [1..30] <*> qList [40..60])
--
concurrentE :: MonadConc f =>
  Queue a -> Emitter f a -> Emitter f a -> CoEmitter f a
concurrentE q e e' =
  Codensity $ \eaction -> snd . fst <$> C.concurrently (queue q (`glue` e) eaction) (queue q (`glue` e') eaction)

-- | run two committers concurrently
--
-- >>> import Data.Functor.Contravariant
-- >>> import Data.Text (pack)
-- >>> cFast = mapC (\b -> pure (Just b)) . contramap ("fast: " <>) $ toStdout
-- >>> cSlow = mapC (\b -> sleep 0.1 >> pure (Just b)) . contramap ("slow: " <>) $ toStdout
-- >>> close $ (popList ((pack . show) <$> [1..3]) <$> (concurrentC Unbounded cFast cSlow)) <> pure (sleep 1)
-- fast: 1
-- fast: 2
-- fast: 3
-- slow: 1
-- slow: 2
-- slow: 3
concurrentC :: (MonadConc m) => Queue a -> Committer m a -> Committer m a -> CoCommitter m a
concurrentC q c c' = mergeC <$> eitherC q c c'

eitherC ::
  (MonadConc m) =>
  Queue a ->
  Committer m a ->
  Committer m a ->
  Codensity m (Either (Committer m a) (Committer m a))
eitherC q cl cr =
  Codensity $
    \kk ->
      fst
        <$> C.concurrently
          (queueL q (kk . Left) (glue cl))
          (queueL q (kk . Right) (glue cr))

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
