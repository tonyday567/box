{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Streaming functionality
module Box.Stream
  ( toStream,
    fromStream,
    toCommit,
    toCommitFold,
    toCommitSink,
    toEmit,
    queueStream,
    toStreamM,
    fromStreamM,
  )
where

import Box.Committer
import Box.Cont
import Box.Emitter
import Box.Queue
import qualified Control.Foldl as L
import Control.Monad.Conc.Class as C
import NumHask.Prelude hiding (STM, atomically)
import Streaming (Of (..), Stream)
import qualified Streaming.Prelude as S

-- * streaming

-- | create a committer from a stream consumer
toCommit :: (MonadConc m) => (Stream (Of a) m () -> m r) -> Cont m (Committer (STM m) a)
toCommit f =
  Cont (\c -> queueC' c (\(Emitter o) -> f . toStream . Emitter $ o))

-- | create a committer from a fold
toCommitFold :: (MonadConc m) => L.FoldM m a () -> Cont m (Committer (STM m) a)
toCommitFold f = toCommit (fmap S.snd' . L.impurely S.foldM f)

-- | create a committer from a sink
toCommitSink :: (MonadConc m) => (a -> m ()) -> Cont m (Committer (STM m) a)
toCommitSink sink = toCommitFold (L.FoldM step begin done)
  where
    step x a = do
      sink a
      pure x
    begin = pure ()
    done = pure

-- | create an emitter from a stream
toEmit :: (MonadConc m) => Stream (Of a) m () -> Cont m (Emitter (STM m) a)
toEmit s = Cont (queueE' (fromStream s))

-- | insert a queue into a stream (left biased collapse)
-- todo: look at biases
queueStream ::
  (MonadConc m) => Stream (Of a) m () -> Cont m (Stream (Of a) m ())
queueStream i = Cont $ \o -> queueE' (fromStream i) (o . toStream)

-- | turn an emitter into a stream
toStream :: (MonadConc m) => Emitter (STM m) a -> Stream (Of a) m ()
toStream e = toStreamM (liftE e)

-- | turn an emitter into a stream
toStreamM :: (MonadConc m) => Emitter m a -> Stream (Of a) m ()
toStreamM e = S.untilRight getNext
  where
    getNext = maybe (Right ()) Left <$> emit e

-- | turn a stream into a committer
fromStream :: (MonadConc m) => Stream (Of b) m () -> Committer (STM m) b -> m ()
fromStream s c = fromStreamM s (liftC c)

-- | turn a stream into a committer
fromStreamM :: (MonadConc m) => Stream (Of b) m () -> Committer m b -> m ()
fromStreamM s c = go s
  where
    go str = do
      eNxt <- S.next str -- uncons requires r ~ ()
      forM_ eNxt $ \(a, str') -> do
        continue <- commit c a
        when continue (go str')
