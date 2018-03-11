{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | plugs
-- box continuations
--
module Etc.Plugs
  ( commitPlug
  , emitPlug
  , boxPlug
  , boxForgetPlug
  ) where

import Control.Category
import Data.Semigroup hiding (First, getFirst)
import Etc.Committer
import Etc.Cont
import Etc.Box
import Etc.Queue
import Etc.Emitter
import GHC.Conc
import Protolude hiding ((.), (<>))

-- * plugs
-- | hook an emitter action to a queue, creating a committer continuation
commitPlug :: (Emitter STM a -> IO ()) -> Cont IO (Committer STM a)
commitPlug eio = Cont $ \cio -> queueC cio eio

-- | hook a committer action to a queue, creating an emitter continuation
emitPlug :: (Committer STM a -> IO r) -> Cont IO (Emitter STM a)
emitPlug cio = Cont $ \eio -> queueE cio eio

-- | create a double-queued box plug
boxPlug ::
     (Emitter STM a -> IO ())
  -> (Committer STM b -> IO ())
  -> Cont IO (Box STM a b)
boxPlug eio cio = Box <$> commitPlug eio <*> emitPlug cio

-- | create a box plug from a box action.  Caution: implicitly, this (has to) forget interactions between emitter and committer in the one action (and it does so silently).  These forgotten interactions are typically those that create races
boxForgetPlug :: (Box STM b a -> IO ()) -> Cont IO (Box STM a b)
boxForgetPlug bio = boxPlug (bio . Box mempty) (bio . (`Box` mempty))
