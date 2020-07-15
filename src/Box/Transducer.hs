{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | `transduce`
module Box.Transducer
  ( Transducer (..),
    etc,
    toCommitter,
    toEmitter,
    toStream,
    fromStream,
    foldCommitter,
    sinkCommitter,
  )
where

import Box.Box
import Box.Committer
import Box.Cont
import Box.Emitter
import Box.Queue
import qualified Control.Foldl as L
import Control.Lens hiding ((.>), (:>), (<|), (|>))
import Control.Monad.Conc.Class as C
import NumHask.Prelude hiding (STM, atomically)
import Streaming (Of (..), Stream)
import qualified Streaming.Prelude as S

-- | transduction
-- [wiki](https://en.wikipedia.org/wiki/Transducer) says: "A transducer is a device that converts energy from one form to another." Translated to context, this Transducer converts a stream of type a to a stream of a different type.
newtype Transducer s a b
  = Transducer
      { transduce ::
          forall m.
          Monad m =>
          Stream (Of a) (StateT s m) () ->
          Stream (Of b) (StateT s m) ()
      }

instance Category (Transducer s) where
  (Transducer t1) . (Transducer t2) = Transducer (t1 . t2)

  id = Transducer id

-- | emit - transduce - commit
--
-- with etc, you're in the box, and inside the box, there are no effects: just a stream of a's, pure functions and state tracking. It's a nice way to code, and very friendly for the compiler. When the committing and emitting is done, the box collapses to state.
--
-- The combination of an input tape, an output tape, and a state-based stream computation lends itself to the etc computation as a [finite-state transducer](https://en.wikipedia.org/wiki/Finite-state_transducer) or mealy machine.
etc :: Monad m => s -> Transducer s a b -> Box m b a -> m s
etc st t (Box c e) =
  (e & hoist lift & toStream & transduce t & fromStream) (hoist lift c) & flip execStateT st

-- | create an committer from a stream continuation
toCommitter :: (MonadConc m) => (Stream (Of a) m () -> m r) -> Cont m (Committer m a)
toCommitter f =
  Cont (\c -> queueC c (\(Emitter o) -> f . toStream . Emitter $ o))

-- | create an emitter from a stream
toEmitter :: (MonadConc m) => Stream (Of b) m () -> Cont m (Emitter m b)
toEmitter s = Cont (queueE (fromStream s))

-- | turn an emitter into a stream
toStream :: (Monad m) => Emitter m a -> Stream (Of a) m ()
toStream e = S.untilRight getNext
  where
    getNext = maybe (Right ()) Left <$> emit e

-- | turn a stream into a committer
fromStream :: (Monad m) => Stream (Of b) m () -> Committer m b -> m ()
fromStream s c = go s
  where
    go str = do
      eNxt <- S.next str -- uncons requires r ~ ()
      forM_ eNxt $ \(a, str') -> do
        continue <- commit c a
        when continue (go str')

-- | create a committer from a fold
foldCommitter :: (MonadConc m) => L.FoldM m a () -> Cont m (Committer m a)
foldCommitter f = toCommitter (fmap S.snd' . L.impurely S.foldM f)

-- | create a committer from a sink
sinkCommitter :: (MonadConc m) => (a -> m ()) -> Cont m (Committer m a)
sinkCommitter sink = foldCommitter (L.FoldM step begin done)
  where
    step x a = do
      sink a
      pure x
    begin = pure ()
    done = pure
