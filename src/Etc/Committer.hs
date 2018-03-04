{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

-- | `commit`
module Etc.Committer
  ( Committer(..)
  , liftC
  , maybeCommit
  , handles
  , cShow
  ) where

import Control.Category
import Control.Lens hiding ((:>), (.>), (<|), (|>))
import Data.Functor.Constant
import Data.Functor.Contravariant.Divisible
import Data.Semigroup hiding (First, getFirst)
import Etc.Cont
import Protolude hiding ((.), (<>))

-- | a Committer a "commits" values of type a. A Sink and a Consumer are some other metaphors for this.
--
-- A Committer 'absorbs' the value being committed; the value disappears into the opaque thing that is a Committer from the pov of usage.
--
newtype Committer m a = Committer
  { commit :: a -> m Bool
  }

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

-- | lift an committer from STM to IO
liftC :: Committer STM a -> Committer IO a
liftC c = Committer $ atomically . commit c

-- | maybe commit based on an action
--
maybeCommit :: (Monad m) => (b -> m (Maybe a)) -> Committer m a -> Committer m b
maybeCommit f c = Committer go
  where
    go b = do
      fb <- f b
      case fb of
        Nothing -> pure True
        Just fb' -> commit c fb'

-- | commit a "show a"
cShow :: (Show a) => Cont IO (Committer STM Text) -> Cont IO (Committer STM a)
cShow c = contramap show <$> c

-- | handle certain commissions
handles ::
     (Monad m)
  => ((b -> Constant (First b) b) -> (a -> Constant (First b) a))
    -- ^
  -> Committer m b
    -- ^
  -> Committer m a
handles k (Committer commit_) =
  Committer
    (\a ->
       case match a of
         Nothing -> return False
         Just b -> commit_ b)
  where
    match = getFirst . getConstant . k (Constant . First . Just)
