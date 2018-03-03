{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | timing effects
module Etc.Time
  ( sleep
  , keepOpen
  , delayTimed
  , Stamped(..)
  , stampNow
  , emitStamp
  ) where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Maybe
import Data.Time
import Etc.Cont
import Etc.Emitter
import Etc.Stream
import Protolude
import qualified Streaming.Prelude as S
import System.IO.Unsafe

-- | sleep for x seconds
sleep :: Double -> IO ()
sleep x = threadDelay (floor $ x * 1e6)

-- | keeping a box open sometimes needs a long running emitter
keepOpen :: Cont IO (Emitter STM a)
keepOpen = toEmit $ lift $ threadDelay (365 * 24 * 60 * 60 * 10 ^ 6)

-- | a stream with suggested delays.  DiffTime is the length of time to wait since the start of the stream
-- > delayTimed (S.each (zip (fromIntegral <$> [1..10]) [1..10])) |> S.print
delayTimed ::
     S.Stream (S.Of (NominalDiffTime, a)) IO () -> S.Stream (S.Of a) IO ()
delayTimed s = do
  t0 <- lift getCurrentTime
  go s t0
  where
    go ::
         S.Stream (S.Of (NominalDiffTime, a)) IO ()
      -> UTCTime
      -> S.Stream (S.Of a) IO ()
    go s t0 = do
      n <- liftIO $ S.uncons s
      case n of
        Nothing -> pure ()
        Just ((t1, a'), s') -> do
          lift $ delayTo (addUTCTime t1 t0)
          S.yield a'
          go s' t0
    delayTo t = do
      now <- getCurrentTime
      let gap = max 0 $ diffUTCTime t now
      liftIO (threadDelay (truncate (gap * 1000000)))

data Stamped a = Stamped
  { timestamp :: UTCTime
  , value :: a
  } deriving (Eq, Show, Read)

stampNow :: a -> IO (Stamped a)
stampNow a = do
  t <- getCurrentTime
  pure $ Stamped t a

-- | adding a time stamp
-- todo: how to do this properly?
emitStamp :: Cont IO (Emitter STM a) -> Cont IO (Emitter STM (Stamped a))
emitStamp e = fmap (unsafePerformIO . stampNow) <$> e
-- | todo: is this possible?
-- commitStamp :: Managed (Committer a) -> Managed (Committer (Stamped a))
-- commitStamp c = managed $ \cio -> with c $ \c -> undefined
