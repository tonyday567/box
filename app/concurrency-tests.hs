{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | dejavu testing
module Main where

import Box
import Control.Lens
import Control.Monad.Conc.Class as C
import qualified Data.Sequence as Seq
import NumHask.Prelude hiding (STM)
import Test.DejaFu hiding (get)
import Test.DejaFu.Types

tERef :: (MonadConc m) => Emitter m a -> m [a]
tERef e = do
  (c, res) <- cRef
  glue c e
  res

tToListE :: (MonadConc m) => Int -> m [Int]
tToListE n =
  toListE <$.> fromListE [1 .. n]

tFromListE :: (MonadConc m) => Int -> m [Int]
tFromListE n = do
  (c, res) <- cRef
  let e = fromListE [0 .. (n - 1)]
  fuse (pure . pure) <$.> (Box c <$> e)
  res

tToList_ :: (MonadConc m) => Int -> m [Int]
tToList_ n =
  toList_ <$.> fromListE [1 .. n]

-- failing
tFromList_ :: (MonadConc m) => Int -> m [Int]
tFromList_ n = do
  (c, res) <- cRef
  fromList_ [1 .. n] c
  res

tFromList_' :: (MonadConc m) => Int -> m [Int]
tFromList_' n = toList <$> execStateT (fromList_ [1 .. n] stateC) Seq.empty

tPureState :: Int -> [Int]
tPureState n =
  toList $
    runIdentity $
      fmap fst $
        flip execStateT (Seq.empty, Seq.fromList [1 .. n]) $
          glue (hoist (zoom _1) stateC) (hoist (zoom _2) stateE)

tPureBoxF f n = fromToList_ [1 .. n] f

-- tForkEmit <$.> (fromListE [1..4])
-- tForkEmit <$.> (toEmitter (S.take 4 $ S.each [1..]))
tForkEmit :: (MonadConc m) => Emitter m b -> m ([b], [b])
tForkEmit e = do
  (c1, r1) <- cRef
  (c2, r2) <- cRef
  let e' = forkEmit e c1
  glue c2 e'
  (,) <$> r1 <*> r2

-- | test when the deterministic takes too long (which is almost always)
t ::
  (MonadIO n, Eq b, Show b, MonadDejaFu n) =>
  ConcT n b ->
  n Bool
t = dejafuWay (randomly (mkStdGen 42) 1000) defaultMemType "" alwaysSame

main :: IO ()
main = do
  let n = 4
  sequence_ $
    autocheck
      <$> [ tToListE n,
            tFromListE n,
            tToList_ n,
            tFromList_' n,
            pure (tPureState n),
            tPureBoxF (fuse (pure . pure)) n,
            tPureBoxF (\(Box c e) -> glue c e) n,
            uncurry (<>) <$> (tForkEmit <$.> fromListE [1 .. n])
          ]
