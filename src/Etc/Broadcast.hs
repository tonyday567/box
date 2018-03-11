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
import Etc.Box
import Etc.Committer
import Etc.Cont
import Etc.Emitter
import Etc.Queue
import Protolude

newtype Broadcaster m a = Broadcaster
  { unBroadcast :: TVar (Committer m a)
  }

broadcast :: IO (Committer STM a, Broadcaster STM a)
broadcast = do
  tvar <- atomically $ newTVar mempty
  let output a = do
        o <- readTVar tvar
        commit o a
  return (Committer output, Broadcaster tvar)

subscribe :: Broadcaster STM a -> Cont IO (Emitter STM a)
subscribe (Broadcaster tvar) = Cont $ \e -> queueE cio e
  where
    cio c = atomically $ modifyTVar' tvar (mappend c)

newtype Funneler m a = Funneler
  { unFunnel :: TVar (Emitter m a)
  }

funnel :: STM (Funneler STM a, Emitter STM a)
funnel = do
  tvar <- newTVar mempty
  let input =
        Emitter $ do
          i <- readTVar tvar
          emit i
  pure (Funneler tvar, input)

widen :: Funneler STM a -> Cont IO (Committer STM a)
widen (Funneler tvar) =
  Cont $ \c -> queueC c $ \e -> atomically $ modifyTVar' tvar (mappend e)
