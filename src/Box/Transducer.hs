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
    etcM,
  )
where

import Box.Box
import Box.Committer
import Box.Cont
import Box.Emitter
import Box.Stream
import Control.Category (Category (..))
import Control.Lens hiding ((.>), (:>), (<|), (|>))
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Conc.Class as C
import Control.Monad.Trans.State.Lazy
import Streaming (Of (..), Stream)
import Prelude hiding ((.), id)

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
etc :: (MonadConc m) => s -> Transducer s a b -> Cont m (Box (C.STM m) b a) -> m s
etc st t box =
  with box $ \(Box c e) ->
    (e & toStream & transduce t & fromStream) c & flip execStateT st

-- | Monadic version of etc.
etcM :: (MonadConc m, MonadBase m m) => s -> Transducer s a b -> Cont m (Box m b a) -> m s
etcM st t box =
  with box $ \(Box c e) ->
    (liftE' e & toStreamM & transduce t & fromStreamM) (liftC' c) & flip execStateT st
  where
    liftC' c = Committer $ liftBase . commit c
    liftE' = Emitter . liftBase . emit
