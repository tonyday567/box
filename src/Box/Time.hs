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
    stampNow,
    stampE,
    Gap,
    gaps,
    fromGaps,
    fromGapsNow,
    gapEffect,
    skip,
    replay,
  gapSkipEffect, speedEffect, speedSkipEffect)
where

import Box.Emitter
import Control.Applicative
import Control.Monad.Conc.Class as C
import Control.Monad.IO.Class
import Data.Fixed (Fixed (MkFixed))
import Data.Time
import Prelude
import Control.Monad.State.Lazy
import Box.Connectors
import Data.Bifunctor
import Data.Bool

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

-- | Convert a (Gap,a) emitter to an a emitter, with delays between emits of the gap.
gapEffect ::
  C.MonadConc m =>
  Emitter m (Gap, a) ->
  Emitter m a
gapEffect as =
  Emitter $ do
    a <- emit as
    case a of
      (Just (s, a')) -> sleep s >> pure (Just a')
      _ -> pure Nothing

speedEffect ::
  C.MonadConc m =>
  Emitter m Gap ->
  Emitter m (Gap, a) ->
  Emitter m a
speedEffect speeds as =
  Emitter $ do
    s <- emit speeds
    a <- emit as
    case (s,a) of
      (Just s', Just (g, a')) -> sleep (g/s') >> pure (Just a')
      _ -> pure Nothing

-- | Only add a Gap effect if greater than the Int emitter
--
-- effect is similar to a fast-forward of the first n emits
gapSkipEffect ::
  C.MonadConc m =>
  Emitter m Int ->
  Emitter m Gap ->
  CoEmitter m Gap
gapSkipEffect n e = evalEmitter 0 $ Emitter $ do
    n' <- lift $ emit n
    e' <- lift $ emit e
    count <- get
    modify (1+)
    case (n', e') of
      (_, Nothing) -> pure Nothing
      (Nothing, _) -> pure Nothing
      (Just n'', Just e'') ->
        pure $ Just (bool e'' 0 (n'' >= count))

-- | Only add a Gap if greater than the Int emitter
--
-- effect is similar to a fast-forward of the first n emits
speedSkipEffect ::
  C.MonadConc m =>
  Emitter m (Int, Gap) ->
  Emitter m (Gap, a) ->
  CoEmitter m a
speedSkipEffect p e = evalEmitter 0 $ Emitter $ do
    p' <- lift $ emit p
    e' <- lift $ emit e
    count <- get
    modify (1+)
    case (p', e') of
      (_, Nothing) -> pure Nothing
      (Nothing, _) -> pure Nothing
      (Just (n,s), Just (g,a)) ->
        sleep (bool (g/s) 0 (n >= count)) >> pure (Just a)


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
