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
    delayByWith,
    delayBy,
    emitIn,
    replay,
  delay)
where

import Box.Codensity
import Box.Emitter
import Control.Applicative
import Control.Monad.Conc.Class as C
import Control.Monad.IO.Class
import Data.Fixed (Fixed (MkFixed))
import Data.Time
import Prelude
import Control.Monad.State.Lazy
import Box.Functor
import Box.Connectors
import Data.Bifunctor
import Data.Bool (bool)

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

-- | Convert emitter stamps to adjusted speed delays
delayByWith :: (C.MonadConc m) => Double -> LocalTime -> Emitter m (LocalTime, a) -> CoEmitter m (Double, a)
delayByWith speed t0 e = evalEmitter t0 $ Emitter $ do
  r <- lift $ emit e
  case r of
    Nothing -> pure Nothing
    Just a' -> do
      t' <- get
      let delta u = fromNominalDiffTime $ diffLocalTime u t' / toNominalDiffTime speed
      put (fst a')
      pure $ Just $ first delta a'

-- | Convert emitter stamps to adjusted speed delays
delay :: (C.MonadConc m) => Double -> Int -> Emitter m (Double, a) -> CoEmitter m a
delay speed skip e = evalEmitter skip $ Emitter $ do
  skip' <- get
  e' <- lift $ emit e
  case e' of
    Nothing -> pure Nothing
    Just (secs, a) -> do
      bool (put (skip' - 1)) (lift $ sleep (secs/speed)) (skip'==0)
      pure (Just a)

-- | Convert emitter stamps to adjusted speed delays
delayBy :: (Alternative m, C.MonadConc m) => Double -> Emitter m (LocalTime, a) -> CoEmitter m (Double, a)
delayBy speed e = Codensity $ \k -> do
  r <- emit e
  case r of
    Nothing -> k mempty
    Just a ->
      close $ k <$> ((<>) <$> source 1 (pure (0,snd a)) <*> delayByWith speed (fst a) e)

-- | Wait s seconds before emitting, skipping delaying the first n emits
emitIn ::
  C.MonadConc m =>
  Int ->
  Emitter m (Double, a) ->
  CoEmitter m a
emitIn skip e =
  evalEmitter skip $
  witherE
    ( \(s, a) -> do
        i <- get
        case i of
          0 -> sleep s
          x -> do
            put (x-1)
        pure (Just a)
    )
    (foist lift e)

-- | Replay a stamped emitter, adjusting the speed of the replay.
--
replay :: (C.MonadConc m, Alternative m) => Double -> Int -> Emitter m (LocalTime, a) -> CoEmitter m a
replay speed skip e = emitIn skip =<< delayBy speed e
