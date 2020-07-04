{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | `commit`
module Box.Committer
  ( Committer (..),
    drain,
    liftC,
    cmap,
    handles,
    addC,
    postaddC,
  )
where

import Control.Lens hiding ((.>), (:>), (<|), (|>))
import Control.Monad.Conc.Class as C
import Data.Functor.Contravariant.Divisible
import NumHask.Prelude hiding (STM, atomically)

-- | a Committer a "commits" values of type a. A Sink and a Consumer are some other metaphors for this.
--
-- A Committer absorbs the value being committed; the value disappears into the opaque thing that is a Committer from the pov of usage.
newtype Committer m a
  = Committer
      { commit :: a -> m Bool
      }

instance MFunctor Committer where
  hoist nat (Committer c) = Committer $ nat . c

instance (Applicative m) => Semigroup (Committer m a) where
  (<>) i1 i2 = Committer (\a -> (||) <$> commit i1 a <*> commit i2 a)

instance (Applicative m) => Monoid (Committer m a) where
  mempty = Committer (\_ -> pure False)

  mappend = (<>)

instance Contravariant (Committer m) where
  contramap f (Committer a) = Committer (a . f)

instance (Applicative m) => Divisible (Committer m) where
  conquer = Committer (\_ -> pure False)

  divide f i1 i2 =
    Committer $ \a ->
      case f a of
        (b, c) -> (||) <$> commit i1 b <*> commit i2 c

instance (Applicative m) => Decidable (Committer m) where
  lose f = Committer (absurd . f)

  choose f i1 i2 =
    Committer $ \a ->
      case f a of
        Left b -> commit i1 b
        Right c -> commit i2 c

-- | Do nothing with values that are committed.
--
-- This is useful for keeping the commit end of a box or pipeline open.
drain :: (Applicative m) => Committer m a
drain = Committer (\_ -> pure True)

-- | lift a committer from STM
liftC :: (MonadConc m) => Committer (STM m) a -> Committer m a
liftC = hoist atomically

-- | This is a contramapMaybe, if such a thing existed, as the contravariant version of a mapMaybe.  See [witherable](https://hackage.haskell.org/package/witherable)
cmap :: (Monad m) => (b -> m (Maybe a)) -> Committer m a -> Committer m b
cmap f c = Committer go
  where
    go b = do
      fb <- f b
      case fb of
        Nothing -> pure True
        Just fb' -> commit c fb'

-- | prism handler
handles ::
  (Monad m) =>
  -- |
  ((b -> Constant (First b) b) -> (a -> Constant (First b) a)) ->
  -- |
  Committer m b ->
  Committer m a
handles k (Committer commit_) =
  Committer
    ( \a ->
        case match a of
          Nothing -> return False
          Just b -> commit_ b
    )
  where
    match = getFirst . getConstant . k (Constant . First . Just)

-- | adds a monadic action to the committer
addC :: (Applicative m) =>
    (Committer m a -> m ())
    -> Committer m a
    -> Committer m a
addC f c = Committer $ \a -> f c *> commit c a

-- | adds a post-commit monadic action to the committer
postaddC :: (Monad m) =>
    (Committer m a -> m ())
    -> Committer m a
    -> Committer m a
postaddC f c = Committer $ \a -> do
  r <- commit c a
  f c
  pure r

