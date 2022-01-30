{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | queues
-- Follows [pipes-concurrency](https://hackage.haskell.org/package/pipes-concurrency)
module Box.SeqQueue
  (
  )
where

import Box.Box
import Box.Committer
import Box.Cont
import Box.Emitter
import Control.Applicative
import Control.Concurrent.Classy.Async as C
import Control.Concurrent.Classy.STM as C
import Control.Monad.Catch as C
import Control.Monad.Conc.Class as C
import Control.Monad.Morph
import Prelude
import Control.Monad.State.Lazy
import Data.Sequence
import Data.Functor

pop :: (Monad m) => StateT (Seq a) m (Maybe a)
pop = do
  xs <- get
  case xs of
    Empty -> pure Nothing
    (x :<| xs') -> do
      put xs'
      pure (Just x)

push :: (Monad m) => a -> StateT (Seq a) m Bool
push a = do
  modify (:|> a)
  pure True

new :: (Monad m) => Box (StateT (Seq a) m) a a
new = Box (Committer push) (Emitter pop)

-- withStateBox :: CoBox m a a
-- withStateBox :: CoBox m a a -> (Box m a a -> m b) -> m b
-- withStateBox :: Monad m =>
--   Codensity m2 a -> (Box (StateT (Seq a) m) a a -> a -> m2 b) -> m2 b
-- withStateBox b k = runCodensity b (k new)

stateBox :: (Monad m) => m (Box m a a)
stateBox = pure (hoistb (`evalStateT` Empty) new)

runBoxSL :: (Monad m) => (Committer m a -> m l) -> (Emitter m a -> m r) -> m l
runBoxSL cio eio = do
  (Box c e) <- stateBox
  l <- cio c
  _ <- eio e
  pure l

runBoxSR :: (Monad m) => (Committer m a -> m l) -> (Emitter m a -> m r) -> m r
runBoxSR cio eio = do
  (Box c e) <- stateBox
  _ <- cio c
  r <- eio e
  pure r

-- runBox :: (Monad m) => (Box m a a -> m r) -> m r
-- runBox :: (MonadTrans t1, Monad m, Monad (Box (StateT (Seq a) m) a)) =>
--   (t1 (Box (StateT (Seq a) m) a) a -> t2) -> t2
runBox :: (Monad m1, Monad m2) =>
  (Box (StateT (Seq a1) m2) a1 a1 -> StateT (Seq a2) m1 a3) -> m1 a3
runBox k = do
  (`evalStateT` Empty) (k new)

fromList_ :: Monad m => [a] -> Committer m a -> m ()
fromList_ xs c = flip evalStateT (fromList xs) $ glue (hoist lift c) stateE

fromListCo xs = Codensity (runBoxSR (fromList_ xs))

