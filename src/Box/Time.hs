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
    sleepUntil,
    toDouble,
    fromDouble,
    delayTimed,
    Stamped (..),
    stampNow,
    emitStamp,
    playback,
    emitWhen,
    simulate,
  )
where

import Box.Cont
import Box.Emitter
import Control.Monad.Conc.Class as C
import Data.Time
import NumHask.Prelude hiding (STM, atomically)
import qualified Streaming as S
import qualified Streaming.Prelude as S
import qualified Prelude as P

-- | sleep for x seconds
sleep :: (MonadConc m) => Double -> m ()
sleep x = C.threadDelay (fromIntegral (floor $ x * 1e6 :: Integer))

sleepUntil :: UTCTime -> IO ()
sleepUntil u = do
  t0 <- getCurrentTime
  sleep (toDouble $ diffUTCTime u t0)

toDouble :: NominalDiffTime -> Double
toDouble t =
  (/ 1000000000000.0) $
    fromIntegral (P.floor $ t P.* 1000000000000 :: Integer)

fromDouble :: Double -> NominalDiffTime
fromDouble x =
  let d0 = ModifiedJulianDay 0
      days = floor (x / toDouble nominalDay)
      secs = x - fromIntegral days * toDouble nominalDay
      t0 = UTCTime d0 (picosecondsToDiffTime 0)
      t1 = UTCTime (addDays days d0) (picosecondsToDiffTime $ floor (secs / 1.0e-12))
   in diffUTCTime t1 t0

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
  (Emitter m a) ->
  (Emitter m (Stamped a))
emitStamp e = emap (fmap Just . stampNow) e

-- | wait until Stamped time before emitting
emitWhen ::
  Emitter IO (Stamped a) ->
  Emitter IO a
emitWhen e =
  emap
    ( \(Stamped u a) -> do
        sleepUntil u
        pure $ Just a
    )
    e

-- | reset the emitter stamps to by in sync with the current time and adjust the speed
-- >>> let e1 = fromListE (zipWith (\x a -> Stamped (addUTCTime (fromDouble x) t) a) [0..5] [0..5])
playback :: Double -> Emitter IO (Stamped a) -> IO (Emitter IO (Stamped a))
playback speed e = do
  r <- emit e
  case r of
    Nothing -> pure mempty
    Just (Stamped u0 _) -> do
      t0 <- getCurrentTime
      let ua = diffUTCTime t0 u0
      let delta u = addUTCTime ua $ addUTCTime (fromDouble ((toDouble $ diffUTCTime u u0) * speed)) u0
      pure (emap (\(Stamped u a) -> pure (Just (Stamped (delta u) a))) e)

-- | simulate a delay from a Stamped a emitter relative to the first timestamp
simulate :: Double -> Emitter IO (Stamped a) -> Cont IO (Emitter IO a)
simulate speed e = Cont $ \eaction -> do
  e' <- playback speed e
  eaction (emitWhen e')
