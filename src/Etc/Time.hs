{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | timing effects
module Etc.Time where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Maybe
import Data.Time
import Etc
import Protolude
import Control.Monad.Managed
import qualified Streaming.Prelude as S
import System.IO.Unsafe

-- | a stream with suggested delays.  DiffTime is the length of time to wait since the start of the stream
delayTimed :: S.Stream (S.Of (NominalDiffTime, a)) IO () -> S.Stream (S.Of a) IO ()
delayTimed s = do
  t0 <- lift getCurrentTime
  go s t0
  where
    go :: S.Stream (S.Of (NominalDiffTime, a)) IO () -> UTCTime -> S.Stream (S.Of a) IO ()
    go s t0 = do
      n <- liftIO $ S.uncons s
      case n of
        Nothing -> pure ()
        Just ((t1, a'),s') -> do
          lift $ delayTo (addUTCTime t1 t0)
          S.yield a'
          go s' t0
    delayTo t = do
      now <- getCurrentTime
      let gap = max 0 $ diffUTCTime t now
      liftIO (threadDelay (truncate (gap * 1000000)))

data Stamped a = Stamped { timestamp :: UTCTime, value :: a} deriving (Eq, Show, Read)

stampNow :: a -> IO (Stamped a)
stampNow a = do
  t <- getCurrentTime
  pure $ Stamped t a

-- | adding a time stamp
-- todo: how to do this properly?
emitStamp :: Managed (Emitter a) -> Managed (Emitter (Stamped a))
emitStamp e = fmap (unsafePerformIO . stampNow) <$> e

-- | todo: is this possible?
-- commitStamp :: Managed (Committer a) -> Managed (Committer (Stamped a))
-- commitStamp c = managed $ \cio -> with c $ \c -> undefined


