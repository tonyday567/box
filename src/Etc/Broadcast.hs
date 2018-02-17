{-# LANGUAGE NoImplicitPrelude #-}

module Etc.Broadcast where

import Control.Concurrent.STM
import Etc
import qualified Pipes.Concurrent as P
import Protolude

newtype Broadcaster a = Broadcaster
  { unBroadcast :: TVar (P.Output a)
  }

broadcast :: STM (Committer a, Broadcaster a)
broadcast = do
  tvar <- newTVar mempty
  let output =
        P.Output $ \a -> do
          o <- readTVar tvar
          P.send o a
  return (Committer output, Broadcaster tvar)

subscribe :: P.Buffer a -> Broadcaster a -> IO (Emitter a)
subscribe buffer (Broadcaster tvar) = do
  (output, input) <- P.spawn buffer
  atomically $ modifyTVar' tvar (mappend output)
  return (Emitter input)

newtype Funneler a = Funneler
  { unFunnnel :: TVar (P.Input a)
  }

funnel :: STM (Funneler a, Emitter a)
funnel = do
  tvar <- newTVar mempty
  let input =
        P.Input $ do
          i <- readTVar tvar
          P.recv i
  return (Funneler tvar, Emitter input)

widen :: P.Buffer a -> Funneler a -> IO (Committer a)
widen buffer (Funneler tvar) = do
  (output, input) <- P.spawn buffer
  atomically $ modifyTVar' tvar (mappend input)
  return (Committer output)
