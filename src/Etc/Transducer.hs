{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | `transduce`
--
-- > import Etc
-- > import qualified Streaming.Prelude as S
-- > let committer' = cStdout 100 unbounded
-- > let emitter' = toEmit (bounded 1) (S.each ["hi","bye","q","x"])
-- > let box' = Box <$> committer' <*> emitter'
-- > let transducer' = Transducer $ \s -> s & S.takeWhile (/="q") & S.map ("echo: " <>)
-- > etc () transducer' box'
-- echo: hi
-- echo: bye
--
module Etc.Transducer
  ( Transducer(..)
  , etc
  , etcM
  , toStream
  , toStreamIO
  , fromStream
  , fromStreamIO
  , asPipe
  ) where

import Control.Category
import Control.Lens hiding ((:>), (.>), (<|), (|>))
import Control.Monad.Base (MonadBase, liftBase)
import Etc.Box
import Etc.Committer
import Etc.Cont
import Etc.Emitter
import Flow
import qualified Pipes
import qualified Pipes.Prelude as Pipes
import Protolude hiding ((.), (<>))
import Streaming (Of(..), Stream)
import qualified Streaming.Prelude as S

-- | transduction
-- [wiki](https://en.wikipedia.org/wiki/Transducer) says: "A transducer is a device that converts energy from one form to another." Translated to context, this Transducer converts a stream of type a to a stream of a different type.
--
newtype Transducer s a b = Transducer
  { transduce :: forall m. Monad m =>
                             Stream (Of a) (StateT s m) () -> Stream (Of b) (StateT s m) ()
  }

instance Category (Transducer s) where
  (Transducer t1) . (Transducer t2) = Transducer (t1 . t2)
  id = Transducer id

-- | convert a Pipe to a Transducer
asPipe ::
     (Monad m)
  => Pipes.Pipe a b (StateT s m) ()
  -> (Stream (Of a) (StateT s m) () -> Stream (Of b) (StateT s m) ())
asPipe p s = ((s & Pipes.unfoldr S.next) Pipes.>-> p) & S.unfoldr Pipes.next

-- | emit - transduce - commit
--
-- >>> etc () transducer' box'
-- echo: hi
-- echo: bye
--
--
-- with etc, you're in the box, and inside the box, there are no effects: just a stream of 'a's, pure functions and state tracking. It's a nice way to code, and very friendly for the compiler. When the committing and emitting is done, the box collapses to state.
--
-- The combination of an input tape, an output tape, and a state-based stream computation lends itself to the etc computation as a [finite-state transducer](https://en.wikipedia.org/wiki/Finite-state_transducer) or mealy machine.
--
etc :: s -> Transducer s a b -> Cont IO (Box STM b a) -> IO s
etc st t box =
  with box $ \(Box c e) ->
    (e |> toStreamIO |> transduce t |> fromStreamIO) c |> flip execStateT st

etcM :: (MonadBase m m) => s -> Transducer s a b -> Cont m (Box m b a) -> m s
etcM st t box =
  with box $ \(Box c e) ->
    (e |> toStreamM |> transduce t |> fromStreamM) c |> flip execStateT st

-- | turn an emitter into a stream
toStreamM :: (MonadBase m n) => Emitter m a -> Stream (Of a) n ()
toStreamM e = S.untilRight getNext
  where
    getNext = maybe (Right ()) Left <$> liftBase (emit e)

-- | turn a stream into a committer
fromStreamM :: (MonadBase m n) => Stream (Of b) n () -> Committer m b -> n ()
fromStreamM s c = go s
  where
    go str = do
      eNxt <- S.next str -- uncons requires r ~ ()
      forM_ eNxt $ \(a, str') -> do
        continue <- liftBase $ commit c a
        when continue (go str')

-- | turn an emitter into a stream
toStream :: (MonadBase STM m) => Emitter STM a -> Stream (Of a) m ()
toStream e = S.untilRight getNext
  where
    getNext = maybe (Right ()) Left <$> liftBase (emit e)

-- | turn an emitter into a stream
toStreamIO :: (MonadBase IO m) => Emitter STM a -> Stream (Of a) m ()
toStreamIO e = S.untilRight getNext
  where
    getNext = maybe (Right ()) Left <$> liftBase (atomically (emit e))

-- | turn a stream into a committer
fromStream :: (MonadBase STM m) => Stream (Of b) m () -> Committer STM b -> m ()
fromStream s c = go s
  where
    go str = do
      eNxt <- S.next str -- uncons requires r ~ ()
      forM_ eNxt $ \(a, str') -> do
        continue <- liftBase $ commit c a
        when continue (go str')

-- | turn a stream into a committer
fromStreamIO ::
     (MonadBase IO m) => Stream (Of b) m () -> Committer STM b -> m ()
fromStreamIO s c = go s
  where
    go str = do
      eNxt <- S.next str -- uncons requires r ~ ()
      forM_ eNxt $ \(a, str') -> do
        continue <- liftBase $ atomically $ commit c a
        when continue (go str')
