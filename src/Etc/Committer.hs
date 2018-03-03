{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

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

-- | a Committer commits values of type a (to the effects void presumably). A Sink for 'a's & a Consumer of 'a's are the other metaphors. It is a newtype wrapper around pipes-concurrency Output to pick up the instances.
--
-- Naming is reversed in comparison to the 'Output' it wraps.  An Committer 'reaches out and absorbs' the value being committed; the value disappears into the opaque thing that is a Committer from the pov of usage.  An Committer is named for its' main action: it commits.
--
-- >>> import Etc.Time
-- >>> with (cStdout 100 unbounded) $ \c -> atomically (commit c "something") >> sleep 1
-- something
--
-- >>> let cDelay = maybeCommit (\b -> sleep 0.1 >> pure (Just b)) <$> liftC <$> cStdout 100 unbounded
-- >>> let cImmediate = liftC <$> cStdout 100 unbounded
-- >>> (etcM () transducer' $ (Box <$> (cImmediate <> cDelay) <*> (liftE <$> emitter'))) >> sleep 1
-- echo: hi
-- echo: hi
-- echo: bye
-- echo: bye
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
-- >>> let c = fmap liftC $ cStdout 10 unbounded
-- >>> let e = fmap liftE $ toEmit (bounded 1) (S.each ["hi","bye","q","x"])
-- >>> let c' = maybeCommit (\a -> if a=="q" then (putStrLn "stolen!" >> sleep 1 >> pure (Nothing)) else (pure (Just a))) <$> c :: Managed IO (Committer IO Text)
-- >>> fuse (pure . pure) $ Box <$> c' <*> e
-- hi
-- bye
-- stolen!
-- x
--
maybeCommit :: (Monad m) => (b -> m (Maybe a)) -> Committer m a -> Committer m b
maybeCommit f c = Committer go
  where
    go b = do
      fb <- f b
      case fb of
        Nothing -> pure True
        Just fb' -> commit c fb'

-- | show an a
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
