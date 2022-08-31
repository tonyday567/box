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
    gaps,
    delayByWith,
    delayBy,
    emitIn,
    replay,
  delay, fromGaps, fromGapsNow, gapEffect, skip, Gap)
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
-- > toListM . stampE <$|> (qList [1..3])
-- [(2022-08-30 01:55:16.517127,1),(2022-08-30 01:55:16.517132,2),(2022-08-30 01:55:16.517135,3)]
-- @
stampE ::
  (MonadConc m, MonadIO m) =>
  Emitter m a ->
  Emitter m (LocalTime, a)
stampE = witherE (fmap Just . stampNow)

-- | Convert emitter stamps to adjusted speed delays
--
-- > t0 <- utcToLocalTime utc <$> getCurrentTime
-- > toListM <$|> (delayByWith 0.001 t0 =<< stampE <$> (qList [1..4]))
-- [(1970.116,1),(2.3e-2,2),(1.1e-2,3),(9.0e-3,4)]
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
--
-- The code below prints out the first 2 elemets of the list without a delay effect, and then prints out the remaining emissions with a 2 second delay.
--
-- > :set -XTupleSections
-- > import Data.Text (pack)
-- > glue (contramap (pack . show) toStdout) <$|> (delay 0.5 2 =<< (qList ((1,) <$> [1..4])))
-- 1
-- 2
-- 3
-- 4
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
--
-- The code below prints out the emissions with a 2 second delay.
--
-- > :set -XTupleSections
-- > import Data.Text (pack)
-- > glue (contramap (pack . show) toStdout) <$|> (delayBy 0.5 =<< (qList ((1,) <$> [1..4])))
-- 1
-- 2
-- 3
-- 4
delayBy :: (Alternative m, C.MonadConc m) => Double -> Emitter m (LocalTime, a) -> CoEmitter m (Double, a)
delayBy speed e = Codensity $ \k -> do
  r <- emit e
  case r of
    Nothing -> k mempty
    Just a ->
      close $ k <$> ((<>) <$> source 1 (pure (0,snd a)) <*> delayByWith speed (fst a) e)


type Gap = Double

-- | Convert stamped emitter to gap between emits in seconds
--
-- > toListM <$|> (gaps =<< (fromGapsNow =<< (qList (zip (0:repeat 1) [1..4]))))
-- [(0.0,1),(1.0,2),(1.0,3),(1.0,4)]
gaps :: (C.MonadConc m) => Emitter m (LocalTime, a) -> CoEmitter m (Gap, a)
gaps e = evalEmitter Nothing $ Emitter $ do
  r <- lift $ emit e
  case r of
    Nothing -> pure Nothing
    Just a' -> do
      t' <- get
      let delta u = maybe 0 (fromNominalDiffTime . diffLocalTime u) t'
      put (Just $ fst a')
      pure $ Just $ first delta a'

-- | Convert gaps in seconds to stamps starting from an initial supplied 'LocalTime'
--
fromGaps :: (C.MonadConc m) => LocalTime -> Emitter m (Gap, a) -> CoEmitter m (LocalTime, a)
fromGaps t0 e = evalEmitter t0 $ Emitter $ do
  r <- lift $ emit e
  case r of
    Nothing -> pure Nothing
    Just a' -> do
      t' <- get
      let t'' = addLocalTime (toNominalDiffTime (fst a')) t'
      put t''
      pure $ Just (t'', snd a')

-- | Convert gaps in seconds to stamps starting with current time
--
-- > toListM <$|> (fromGapsNow =<< (qList (zip (0:repeat 1) [1..4])))
-- [(2022-08-30 22:57:33.835228,1),(2022-08-30 22:57:34.835228,2),(2022-08-30 22:57:35.835228,3),(2022-08-30 22:57:36.835228,4)]
fromGapsNow :: (MonadIO m, MonadConc m) => Emitter m (Gap, a) -> CoEmitter m (LocalTime, a)
fromGapsNow e = do
  t0 <- liftIO getCurrentTime
  fromGaps (utcToLocalTime utc t0) e

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

-- | Wait s seconds before emitting
gapEffect ::
  C.MonadConc m =>
  Emitter m (Gap, a) ->
  Emitter m a
gapEffect =
  witherE
    ( \(s, a) -> do
        sleep s
        pure (Just a)
    )

skip :: (C.MonadConc m) => Int -> Emitter m (Gap, a) -> CoEmitter m (Gap, a)
skip sk e = evalEmitter (sk+1) $ Emitter $ do
  skip' <- get
  e' <- lift $ emit e
  case e' of
    Nothing -> pure Nothing
    Just (secs, a) -> do
      case skip' of
        0 -> pure (Just (secs,a))
        _ -> do
          put (skip' - 1)
          pure (Just (0,a))

-- | Replay a stamped emitter, adjusting the speed of the replay.
--
-- > toListM . stampE <$|> (replay 0.1 1 =<< (fromGapsNow =<< (qList (zip (0:repeat 1) [1..4]))))
-- [(2022-08-31 02:29:39.643831,1),(2022-08-31 02:29:39.643841,2),(2022-08-31 02:29:39.746998,3),(2022-08-31 02:29:39.849615,4)]
replay :: (C.MonadConc m) => Double -> Int -> Emitter m (LocalTime, a) -> CoEmitter m a
replay speed sk e = gapEffect . fmap (first (speed*)) <$> (skip sk =<< gaps e)
