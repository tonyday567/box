{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | timing effects
module Box.Time
  ( sleep,
    keepOpen,
    delayTimed,
    Stamped (..),
    stampNow,
    emitStamp,
  )
where

import Box.Cont
import Box.Emitter
import Box.Stream
import Control.Monad.Conc.Class as C
import Data.Time
import NumHask.Prelude hiding (STM, atomically)
import qualified Streaming as S
import qualified Streaming.Prelude as S
import qualified Prelude as P

-- | sleep for x seconds
sleep :: (MonadConc m) => Double -> m ()
sleep x = C.threadDelay (fromIntegral (floor $ x * 1e6 :: Integer))

-- | keeping a box open sometimes needs a long running emitter
keepOpen :: (MonadConc m) => Cont m (Emitter (STM m) a)
keepOpen = toEmit $ lift $ sleep (365 * 24 * 60 * 60)

-- | a stream with suggested delays.  DiffTime is the length of time to wait since the start of the stream
-- > delayTimed (S.each (zip (fromIntegral <$> [1..10]) [1..10])) |> S.print
delayTimed ::
  (MonadConc m, MonadIO m) =>
  S.Stream (S.Of (NominalDiffTime, a)) m () ->
  S.Stream (S.Of a) m ()
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
      sleep (toDouble gap)
    toDouble :: NominalDiffTime -> Double
    toDouble t =
      (/ 1000000000000.0) $
        fromIntegral (P.floor $ t P.* 1000000000000 :: Integer)

-- | A value with a timestamp annotation.
data Stamped a
  = Stamped
      { timestamp :: UTCTime,
        value :: a
      }
  deriving (Eq, Show, Read)

-- | Add the current time
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
