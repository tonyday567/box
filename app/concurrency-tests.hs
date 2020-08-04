{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | dejavu testing
module Main where

import Box
import Control.Lens
import Control.Monad.Conc.Class as C
import NumHask.Prelude hiding (STM)
import System.Random
import Test.DejaFu hiding (get)
import Test.DejaFu.Types

tERef :: (MonadConc m) => Emitter m a -> m [a]
tERef e = do
  (c, res) <- cRef
  glue c e
  res

tEState :: (Monad m) => Emitter m a -> m [a]
tEState e = flip execStateT [] $ glue stateC (hoist lift e)

tToListE :: (MonadConc m) => Int -> m [Int]
tToListE n =
  toListE <$.> fromListE [1 .. n]

tFromListE :: (MonadConc m) => Int -> m [Int]
tFromListE n = do
  (c, res) <- cRef
  let e = fromListE [0 .. (n - 1)]
  fuse (pure . pure) <$.> (Box <$> pure c <*> e)
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
tFromList_' n = reverse <$> (flip execStateT [] $ fromList_ [1 .. n] stateC)

tPureState :: Int -> [Int]
tPureState n =
  runIdentity $ fmap (reverse . fst) $ flip execStateT ([], [1 .. n]) $ glue (hoist (zoom _1) stateC) (hoist (zoom _2) stateE)

tPureBoxF f n =
  fmap (reverse . fst) $ flip execStateT ([], [1 .. n]) $ f (Box (hoist (zoom _1) stateC) (hoist (zoom _2) stateE))

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
t c = dejafuWay (randomly (mkStdGen 42) 1000) defaultMemType "" alwaysSame c

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
            (\(a, b) -> a <> b) <$> (tForkEmit <$.> (fromListE [1 .. n]))
          ]
