{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | A continuation type.
module Box.Cont
  ( Codensity (..),
    close,
    process,
    (<$|>),
    (<*|>),
  )
where

import Control.Applicative
import Control.Monad.Codensity
import Prelude

instance (Semigroup a) => Semigroup (Codensity m a) where
  (<>) = liftA2 (<>)

instance (Functor m, Semigroup a, Monoid a) => Monoid (Codensity m a) where
  mempty = pure mempty

  mappend = (<>)

-- | close a continuation
close :: Codensity m (m r) -> m r
close x = runCodensity x id

-- | flipped runCodensity
--
-- This is equivalent to \f a -> f =<< lowerCodensity a (which is also CoKleisi),
-- but without any Monad constraint.
--
-- The . position is towards the continuation
process :: (a -> m r) -> Codensity m a -> m r
process = flip runCodensity

infixr 3 <$|>

-- | map over a continuation and close.
--
(<$|>) :: (a -> m r) -> Codensity m a -> m r
(<$|>) = process


infixr 3 <*|>

-- | apply to the continuation and close.
--
-- The | is positioned towards the continuation
(<*|>) :: Codensity m (a -> m r) -> Codensity m a -> m r
(<*|>) f a = close (f <*> a)
