{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Streaming functionality
--
module Etc.Stream
  ( toStream
  , fromStream
  , toCommit
  , toCommitFold
  , toCommitIO
  , toEmit
  , queueStream
  ) where

import Control.Category
import Control.Monad.Base (MonadBase)
import Etc.Box
import Etc.Committer
import Etc.Cont
import Etc.Emitter
import Etc.Transducer
import Flow
import Protolude hiding ((.), (<>))
import Streaming (Of(..), Stream)
import qualified Control.Foldl as L
import qualified Streaming.Prelude as S

-- * streaming
-- | create a committer from a stream consumer
toCommit :: (Stream (Of a) IO () -> IO r) -> Cont IO (Committer STM a)
toCommit f =
  Cont (\c -> queueC c (\(Emitter o) -> f . toStreamIO . Emitter $ o))

-- | create a committer from a fold
toCommitFold :: L.FoldM IO a () -> Cont IO (Committer STM a)
toCommitFold f = toCommit (L.impurely S.foldM f .> fmap S.snd')

-- | create a committer from an IO sink
toCommitIO :: (a -> IO ()) -> Cont IO (Committer STM a)
toCommitIO sink = toCommitFold (L.FoldM step begin done)
  where
    step x a = do
      sink a
      pure x
    begin = pure ()
    done = pure

-- | create an emitter from a stream
toEmit :: Stream (Of a) IO () -> Cont IO (Emitter STM a)
toEmit s = Cont (queueE (fromStreamIO s))

-- | insert a queue into a stream (left biased collapse)
-- todo: look at biases
queueStream ::
     (MonadBase IO m) => Stream (Of a) IO () -> Cont IO (Stream (Of a) m ())
queueStream i = Cont $ \o -> queueE (fromStreamIO i) (toStreamIO .> o)
