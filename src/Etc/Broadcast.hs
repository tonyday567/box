{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module Etc.Broadcast
  ( Broadcaster(..)
  , broadcast
  , subscribe
  , Funneler(..)
  , funnel
  , widen
  ) where

import Control.Concurrent.STM
import Etc
import Protolude
import Control.Monad.Managed

newtype Broadcaster a = Broadcaster
  { unBroadcast :: TVar (Committer a)
  }

broadcast :: IO (Committer a, Broadcaster a)
broadcast = do
  tvar <- atomically $ newTVar mempty
  let output a = do
          o <- atomically $ readTVar tvar
          commit o a
  return (Committer output, Broadcaster tvar)

subscribe :: Buffer a -> Broadcaster a -> Managed (Emitter a)
subscribe b (Broadcaster tvar) = managed $ \e -> withBufferC b cio e where
  cio c = atomically $ modifyTVar' tvar (mappend c)

newtype Funneler a = Funneler
  { unFunnel :: TVar (Emitter a)
  }

funnel :: STM (Funneler a, Emitter a)
funnel = do
  tvar <- newTVar mempty
  let input =
        Emitter $ do
          i <- atomically $ readTVar tvar
          emit i
  return (Funneler tvar, input)

widen :: Buffer a -> Funneler a -> Managed (Committer a)
widen b (Funneler tvar) = managed $ \c -> withBufferE b c $ \e ->
  atomically $ modifyTVar' tvar (mappend e)
