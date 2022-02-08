{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | `commit`
module Box.Committer
  ( Committer (..),
    CoCommitter,
    mapC,
    listC,
    push,
  )
where

import Control.Monad.State.Lazy
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import qualified Data.Sequence as Seq
import Data.Void
import Prelude
import Box.Cont (Codensity)
import Box.Functor

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Box
-- >>> import Prelude

-- | a Committer a "commits" values of type a and signals success or otherwise. A Sink and a Consumer are some other metaphors for this.
--
-- A Committer absorbs the value being committed; the value disappears into the opaque thing that is a Committer from the pov of usage.
--
-- >>> commit toStdout "I'm committed!"
-- I'm committed!
-- True
--
newtype Committer m a = Committer
  { commit :: a -> m Bool
  }

-- | cps version of a committer.
type CoCommitter m a = Codensity m (Committer m a)

instance FFunctor Committer where
  foist nat (Committer c) = Committer $ nat . c

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

-- | This is a contramapMaybe, if such a thing existed, as the contravariant version of a mapMaybe.  See [witherable](https://hackage.haskell.org/package/witherable)
mapC :: (Monad m) => (b -> m (Maybe a)) -> Committer m a -> Committer m b
mapC f c = Committer go
  where
    go b = do
      fb <- f b
      case fb of
        Nothing -> pure True
        Just fb' -> commit c fb'

-- | list committer
listC :: (Monad m) => Committer m a -> Committer m [a]
listC c = Committer $ \as ->
  or <$> sequence (commit c <$> as)

-- | push to to a StateT 'Seq'.
push :: (Monad m) => Committer (StateT (Seq.Seq a) m) a
push = Committer $ \a -> do
  modify (Seq.:|> a)
  pure True
