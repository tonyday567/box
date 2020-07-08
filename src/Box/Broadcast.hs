{-# OPTIONS_GHC -Wall #-}

-- | This module is experimental and may not work.
module Box.Broadcast
  ( Broadcaster (..),
    broadcast,
    subscribe,
    Funneler (..),
    funnel,
    widen,
  )
where

import Box.Committer
import Box.Cont
import Box.Emitter
import Box.Queue
import Control.Concurrent.Classy.STM as C
import Control.Monad.Conc.Class as C
import NumHask.Prelude hiding (STM, atomically)

-- | a broadcaster
newtype Broadcaster m a
  = Broadcaster
      { unBroadcast :: TVar (STM m) (Committer m a)
      }

-- | create a (broadcaster, committer)
broadcast :: (MonadConc m) => m (Broadcaster m a, Committer m a)
broadcast = do
  ref <- atomically $ newTVar mempty
  let com = Committer $ \a -> do
        c <- atomically $ readTVar ref
        commit c a
  return (Broadcaster ref, com)

-- | subscribe to a broadcaster
subscribe :: (MonadConc m) => Broadcaster m a -> Cont m (Emitter m a)
subscribe (Broadcaster tvar) = Cont $ \e -> queueE cio e
  where
    cio c = atomically $ modifyTVar' tvar (mappend c)

-- | a funneler
newtype Funneler m a
  = Funneler
      { unFunnel :: TVar (STM m) (Emitter m a)
      }

-- | create a (funneler, emitter)
funnel :: (Alternative m, MonadConc m) => m (Funneler m a, Emitter m a)
funnel = do
  ref <- atomically $ newTVar mempty
  let em =
        Emitter $ do
          e <- atomically $ readTVar ref
          emit e
  pure (Funneler ref, em)

-- | widen to a funneler
widen :: (Alternative m, MonadConc m) => Funneler m a -> Cont m (Committer m a)
widen (Funneler tvar) =
  Cont $ \c -> queueC c $ \e -> atomically $ modifyTVar' tvar (mappend e)
