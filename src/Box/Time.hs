{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | timing effects
module Box.Time
  ( sleep
  , keepOpen
  , delayTimed
  , Stamped(..)
  , stampNow
  , emitStamp
  ) where

import Data.Time
import Box.Cont
import Box.Emitter
import Box.Stream
import qualified Streaming.Prelude as S
import qualified Streaming as S
import Control.Monad.Conc.Class as C
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | sleep for x seconds
sleep :: (MonadConc m) => Double -> m ()
sleep x = threadDelay (floor $ x * 1e6)

-- | keeping a box open sometimes needs a long running emitter
keepOpen :: (MonadConc m) => Cont m (Emitter (STM m) a)
keepOpen = toEmit $ lift $ threadDelay (365 * 24 * 60 * 60 * 10 ^ 6)

-- | a stream with suggested delays.  DiffTime is the length of time to wait since the start of the stream
-- > delayTimed (S.each (zip (fromIntegral <$> [1..10]) [1..10])) |> S.print
delayTimed :: (MonadConc m, MonadIO m) =>
     S.Stream (S.Of (NominalDiffTime, a)) m () -> S.Stream (S.Of a) m ()
delayTimed s = do
  t0 <- liftIO getCurrentTime
  go (S.hoistUnexposed lift s) t0
  where
    go s t0 = do
      n <- S.uncons s
      case n of
        Nothing -> pure ()
        Just ((t1, a'), s') -> do
          lift $ delayTo (addUTCTime t1 t0)
          S.yield a'
          go s' t0
    delayTo t = do
      now <- liftIO getCurrentTime
      let gap = max 0 $ diffUTCTime t now
      -- sleep gap
      threadDelay (truncate (gap * 1000000))

data Stamped a = Stamped
  { timestamp :: UTCTime
  , value :: a
  } deriving (Eq, Show, Read)

stampNow :: (MonadConc m, MonadIO m) => a -> m (Stamped a)
stampNow a = do
  t <- liftIO getCurrentTime
  pure $ Stamped t a

-- | adding a time stamp
-- todo: how to do this properly?
emitStamp ::
  (MonadConc m, MonadIO m) =>
  Cont m (Emitter m a) ->
  Cont m (Emitter m (Stamped a))
emitStamp e = emap (fmap Just . stampNow) <$> e


