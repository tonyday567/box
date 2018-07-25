{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module Box.Broadcast
  ( Broadcaster(..)
  , broadcast
  , subscribe
  , Funneler(..)
  , funnel
  , widen
  ) where

-- import Control.Concurrent.STM
import Box.Committer
import Box.Cont
import Box.Emitter
import Box.Queue
-- import Protolude
import Protolude hiding (STM(..), atomically, (.), (<>))
import Control.Concurrent.Classy.STM as C
import Control.Monad.Conc.Class as C
import Control.Concurrent.Classy.Async as C
import Control.Monad.Catch as C

-- | a broadcaster 
newtype Broadcaster m a = Broadcaster
  { unBroadcast :: TVar m (Committer m a)
  }

-- | create a (broadcaster, committer)
broadcast :: (MonadSTM stm) => stm (Broadcaster stm a, Committer stm a)
broadcast = do
  ref <- newTVar mempty
  let com = Committer $ \a -> do
        c <- readTVar ref
        commit c a
  return (Broadcaster ref, com)

-- | subscribe to a broadcaster
subscribe :: (MonadConc m) => Broadcaster (STM m) a -> Cont m (Emitter (STM m) a)
subscribe (Broadcaster tvar) = Cont $ \e -> queueE cio e
  where
    cio c = atomically $ modifyTVar' tvar (mappend c)

-- | a funneler
newtype Funneler m a = Funneler
  { unFunnel :: TVar m (Emitter m a)
  }

-- | create a (funneler, emitter)
funnel :: (MonadSTM stm) => stm (Funneler stm a, Emitter stm a)
funnel = do
  ref <- newTVar mempty
  let em =
        Emitter $ do
          e <- readTVar ref
          emit e
  pure (Funneler ref, em)

-- | widen to a funneler
widen :: (MonadConc m) => Funneler (STM m) a -> Cont m (Committer (STM m) a)
widen (Funneler tvar) =
  Cont $ \c -> queueC c $ \e -> atomically $ modifyTVar' tvar (mappend e)
