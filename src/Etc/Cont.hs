{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Etc.Cont
  ( Cont(..)
  , with
  ) where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Monoid (Monoid(..))

import Data.Semigroup (Semigroup(..))

-- | A continuation
newtype Cont m a = Cont
  { (>>-) :: forall r. (a -> m r) -> m r
  }

instance Functor (Cont m) where
  fmap f mx = Cont (\return_ -> mx >>- \x -> return_ (f x))

instance Applicative (Cont m) where
  pure r = Cont (\return_ -> return_ r)
  mf <*> mx = Cont (\return_ -> mf >>- \f -> mx >>- \x -> return_ (f x))

instance Monad (Cont m) where
  return r = Cont (\return_ -> return_ r)
  ma >>= f = Cont (\return_ -> ma >>- \a -> f a >>- \b -> return_ b)

instance (MonadIO m) => MonadIO (Cont m) where
  liftIO m =
    Cont
      (\return_ -> do
         a <- liftIO m
         return_ a)

instance (Semigroup a) => Semigroup (Cont m a) where
  (<>) = liftA2 (<>)

instance (Functor m, Semigroup a, Monoid a) => Monoid (Cont m a) where
  mempty = pure mempty
  mappend = (<>)

with :: Cont m a -> (a -> m r) -> m r
with = (>>-)
