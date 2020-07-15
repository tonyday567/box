{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

-- | A continuation type.
module Box.Cont
  ( Cont (..),
    runCont,
    (<$.>),
    (<*.>),
    Cont_ (..),
    runCont_,
  )
where

import NumHask.Prelude hiding (STM, atomically)

-- | A continuation similar to ` Control.Monad.ContT` but where the result type is swallowed by an existential
newtype Cont m a
  = Cont
      { with :: forall r. (a -> m r) -> m r
      }

instance Functor (Cont m) where
  fmap f mx = Cont (\return_ -> mx `with` \x -> return_ (f x))

instance Applicative (Cont m) where
  pure r = Cont (\return_ -> return_ r)

  mf <*> mx = Cont (\return_ -> mf `with` \f -> mx `with` \x -> return_ (f x))

instance Monad (Cont m) where
  return r = Cont (\return_ -> return_ r)

  ma >>= f = Cont (\return_ -> ma `with` \a -> f a `with` \b -> return_ b)

instance (MonadIO m) => MonadIO (Cont m) where
  liftIO m =
    Cont
      ( \return_ -> do
          a <- liftIO m
          return_ a
      )

instance (Semigroup a) => Semigroup (Cont m a) where
  (<>) = liftA2 (<>)

instance (Functor m, Semigroup a, Monoid a) => Monoid (Cont m a) where
  mempty = pure mempty

  mappend = (<>)

-- | finally run a continuation
runCont :: Cont m (m r) -> m r
runCont x = with x id

-- | sometimes you have no choice but to void it up
newtype Cont_ m a
  = Cont_
      { with_ :: (a -> m ()) -> m ()
      }

instance Functor (Cont_ m) where
  fmap f mx = Cont_ (\return_ -> mx `with_` \x -> return_ (f x))

instance Applicative (Cont_ m) where
  pure r = Cont_ (\return_ -> return_ r)

  mf <*> mx = Cont_ (\return_ -> mf `with_` \f -> mx `with_` \x -> return_ (f x))

instance Monad (Cont_ m) where
  return r = Cont_ (\return_ -> return_ r)

  ma >>= f = Cont_ (\return_ -> ma `with_` \a -> f a `with_` \b -> return_ b)

instance (MonadIO m) => MonadIO (Cont_ m) where
  liftIO m =
    Cont_
      ( \return_ -> do
          a <- liftIO m
          return_ a
      )

instance (Semigroup a) => Semigroup (Cont_ m a) where
  (<>) = liftA2 (<>)

instance (Functor m, Semigroup a, Monoid a) => Monoid (Cont_ m a) where
  mempty = pure mempty

  mappend = (<>)

-- | finally run a Cont_
runCont_ :: Cont_ m (m ()) -> m ()
runCont_ x = with_ x id

infixr 3 <$.>

-- | fmap over a continuation and then run the result.
--
-- The . position is towards the continuation
(<$.>) :: (a -> m r) -> Cont m a -> m r
(<$.>) f a = runCont (fmap f a)

infixr 3 <*.>

-- | fmap over a continuation and then run the result.
--
-- The . position is towards the continuation
(<*.>) :: Cont m (a -> m r) -> Cont m a -> m r
(<*.>) f a = runCont (f <*> a)
