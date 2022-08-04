{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Extra Codensity operators.
module Box.Codensity
  ( close,
    process,
    (<$|>),
    (<*|>),
    module Control.Monad.Codensity,
  )
where

import Control.Applicative
import Control.Monad.Codensity
import Prelude

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Prelude
-- >>> import Box
-- >>> import Data.Bool

instance (Semigroup a) => Semigroup (Codensity m a) where
  (<>) = liftA2 (<>)

instance (Functor m, Semigroup a, Monoid a) => Monoid (Codensity m a) where
  mempty = pure mempty

  mappend = (<>)

-- | close a continuation
--
-- >>> close $ glue showStdout <$> qList [1..3]
-- 1
-- 2
-- 3
close :: Codensity m (m r) -> m r
close x = runCodensity x id

-- | fmap then close over a Codensity
--
-- >>> process (glue showStdout) (qList [1..3])
-- 1
-- 2
-- 3
process :: forall a m r. (a -> m r) -> Codensity m a -> m r
process f k = runCodensity k f

infixr 0 <$|>

-- | fmap then close over a Codensity
--
-- >>> glue showStdout <$|> qList [1..3]
-- 1
-- 2
-- 3
(<$|>) :: forall a m r. (a -> m r) -> Codensity m a -> m r
(<$|>) = process

infixr 3 <*|>

-- | apply to a continuation and close.
--
-- >>> glue <$> (pure showStdout) <*|> qList [1..3]
-- 1
-- 2
-- 3
(<*|>) :: Codensity m (a -> m r) -> Codensity m a -> m r
(<*|>) f a = close (f <*> a)
