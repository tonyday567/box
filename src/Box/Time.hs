{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | timing effects
module Box.Time
  ( sleep,
    sleepUntil,
    Stamped (..),
    stampNow,
    stampE,
    emitOn,
    playback,
    simulate,
  )
where

import Box.Cont
import Box.Emitter
import Control.Monad.Conc.Class as C
import Data.Time
import NumHask.Prelude hiding (STM, atomically)
import NumHask.Space.Time

-- | sleep for x seconds
sleep :: (MonadConc m) => Double -> m ()
sleep x = C.threadDelay (floor $ x * 1e6)

-- | sleep until a certain time (in the future)
sleepUntil :: UTCTime -> IO ()
sleepUntil u = do
  t0 <- getCurrentTime
  sleep (fromNominalDiffTime $ diffUTCTime u t0)

-- | A value with a UTCTime annotation.
data Stamped a = Stamped
  { stamp :: !UTCTime,
    value :: !a
  }
  deriving (Eq, Show, Read)

-- | Add the current time
stampNow :: (MonadConc m, MonadIO m) => a -> m (LocalTime, a)
stampNow a = do
  t <- liftIO getCurrentTime
  pure (utcToLocalTime utc t, a)

-- | adding a time stamp
stampE ::
  (MonadConc m, MonadIO m) =>
  Emitter m a ->
  Emitter m (LocalTime, a)
stampE = mapE (fmap Just . stampNow)

-- | wait until Stamped time before emitting
emitOn ::
  Emitter IO (LocalTime, a) ->
  Emitter IO a
emitOn =
  mapE
    ( \(l, a) -> do
        sleepUntil (localTimeToUTC utc l)
        pure $ Just a
    )

-- | reset the emitter stamps to by in sync with the current time and adjust the speed
-- >>> let e1 = fromListE (zipWith (\x a -> Stamped (addUTCTime (fromDouble x) t) a) [0..5] [0..5])
playback :: Double -> Emitter IO (LocalTime, a) -> IO (Emitter IO (LocalTime, a))
playback speed e = do
  r <- emit e
  case r of
    Nothing -> pure mempty
    Just (l0, _) -> do
      t0 <- getCurrentTime
      let ua = diffLocalTime (utcToLocalTime utc t0) l0
      let delta u = addLocalTime ua $ addLocalTime (toNominalDiffTime (fromNominalDiffTime (diffLocalTime u l0) * speed)) l0
      pure (mapE (\(l, a) -> pure (Just (delta l, a))) e)

-- | simulate a delay from a (Stamped a) Emitter relative to the first timestamp
simulate :: Double -> Emitter IO (LocalTime, a) -> Cont IO (Emitter IO a)
simulate speed e = Cont $ \eaction -> do
  e' <- playback speed e
  eaction (emitOn e')
