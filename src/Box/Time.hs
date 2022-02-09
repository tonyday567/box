{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Timing effects.
module Box.Time
  ( sleep,
    Stamped (..),
    stampNow,
    stampE,
    emitIn,
    replay,
  )
where

import Box.Codensity
import Box.Emitter
import Control.Monad.Conc.Class as C
import Control.Monad.IO.Class
import Data.Fixed (Fixed (MkFixed))
import Data.Time
import Prelude
import Control.Applicative

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Box
-- >>> import Prelude

-- | Sleep for x seconds.
sleep :: (MonadConc m) => Double -> m ()
sleep x = C.threadDelay (floor $ x * 1e6)

-- | convenience conversion to Double
fromNominalDiffTime :: NominalDiffTime -> Double
fromNominalDiffTime t = fromInteger i * 1e-12
  where
    (MkFixed i) = nominalDiffTimeToSeconds t

-- | convenience conversion from Double
toNominalDiffTime :: Double -> NominalDiffTime
toNominalDiffTime x =
  let d0 = ModifiedJulianDay 0
      days = floor (x / fromNominalDiffTime nominalDay)
      secs = x - fromIntegral days * fromNominalDiffTime nominalDay
      t0 = UTCTime d0 (picosecondsToDiffTime 0)
      t1 = UTCTime (addDays days d0) (picosecondsToDiffTime $ floor (secs / 1.0e-12))
   in diffUTCTime t1 t0

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

-- | Add the current time stamp.
--
-- @
-- > process (toListM . stampE) (qList [1..3])
-- [(2022-02-09 01:18:00.293883,1),(2022-02-09 01:18:00.293899,2),(2022-02-09 01:18:00.293903,3)]
-- @
stampE ::
  (MonadConc m, MonadIO m) =>
  Emitter m a ->
  Emitter m (LocalTime, a)
stampE = witherE (fmap Just . stampNow)

-- | Wait s seconds before emitting
emitIn ::
  Emitter IO (Double, a) ->
  Emitter IO a
emitIn =
  witherE
    ( \(s, a) -> do
        sleep s
        pure $ Just a
    )

-- | Convert emitter stamps to adjusted speed delays
delay :: (Monad m, Alternative m) => Double -> Emitter m (LocalTime, b) -> m (Emitter m (Double, b))
delay speed e = do
  r <- emit e
  case r of
    Nothing -> pure mempty
    Just (t0, _) -> do
      let delta u = fromNominalDiffTime $ diffLocalTime u t0 * toNominalDiffTime speed
      pure (witherE (\(l, a) -> pure (Just (delta l, a))) e)

-- | Replay a stamped emitter, adjusting the speed of the replay.
--
-- @
-- > glueN 4 showStdout <$|> replay 1 (Emitter $ sleep 0.1 >> Just <$> stampNow ())
-- @
replay :: Double -> Emitter IO (LocalTime, a) -> CoEmitter IO a
replay speed e = Codensity $ \eaction -> do
  e' <- delay speed e
  eaction (emitIn e')
