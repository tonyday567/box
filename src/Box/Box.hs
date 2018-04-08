{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | A box is something that commits and emits
--
module Box.Box
  ( Box(..)
  , liftB
  , safeIOToSTM
  ) where

import Control.Category
import qualified Control.Exception
import Control.Lens hiding ((:>), (.>), (<|), (|>))
import Data.Semigroup hiding (First, getFirst)
import Box.Committer
import Box.Emitter
import GHC.Conc
import Protolude hiding ((.), (<>))

-- | A Box is a product of a Committer m and an Emitter. Think of a box with an incoming wire and an outgoing wire. Now notice that the abstraction is reversable: are you looking at two wires from "inside a box"; a blind erlang grunt communicating with the outside world via the two thin wires, or are you looking from "outside the box"; interacting with a black box object. Either way, it's a box.
-- And either way, the committer is contravariant and the emitter covariant so it forms a profunctor.
--
-- a Box can also be seen as having an input tape and output tape, thus available for turing and finite-state machine metaphorics.
--
data Box m c e = Box
  { committer :: Committer m c
  , emitter :: Emitter m e
  }

instance (Functor m) => Profunctor (Box m) where
  dimap f g (Box c e) = Box (contramap f c) (fmap g e)

instance (Alternative m, Monad m) => Semigroup (Box m c e) where
  (<>) (Box c e) (Box c' e') = Box (c <> c') (e <> e')

instance (Alternative m, Monad m) => Monoid (Box m c e) where
  mempty = Box mempty mempty
  mappend = (<>)

-- | lift a box from STM to IO
liftB :: Box STM a b -> Box IO a b
liftB (Box c e) = Box (liftC c) (liftE e)

safeIOToSTM :: IO a -> STM a
safeIOToSTM req =
  unsafeIOToSTM $ do
    tv <- newEmptyMVar
    _ <-
      forkIO $
      ((putMVar tv . Right) =<< req) `Control.Exception.catch`
      (\(e :: SomeException) -> putMVar tv $ Left e)
    r <- takeMVar tv
    case r of
      Right x -> return x
      Left e -> Control.Exception.throw e
