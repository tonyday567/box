{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | A box is something that commits and emits
module Box.Box
  ( BoxF (..),
    Box,
    bmap,
    hoistb,
    hmap',
    hmap2',
    glue,
    glue_,
    glueb,
    fuse,
    dotb,
    Divap (..),
    DecAlt (..),
  )
where

import Box.Committer
import Box.Emitter
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Profunctor
import Prelude
import Control.Monad.Morph
import Control.Applicative
import Data.Void
import Control.Monad
import Data.Functor
import Data.Bool
import Yaya.Functor
import Data.Distributive

-- | A BoxF is a product of a Committer f and an Emitter g.
--
data BoxF f g c e = Box
  { committer :: Committer f c,
    emitter :: Emitter g e
  }

instance (Functor f, Functor g) => Profunctor (BoxF f g) where
  dimap f g (Box c e) = Box (contramap f c) (fmap g e)

type Box f c e = BoxF f f c e

-- | Wrong signature for the MFunctor class
hoistb :: Monad m => (forall a. m a -> n a) -> Box m c e -> Box n c e
hoistb nat (Box c e) = Box (hoist nat c) (hoist nat e)

-- wrong shape
-- instance HFunctor Box where
hmap' :: (forall a. m a -> n a) -> Box m c e -> Box n c e
hmap' nat (Box c e) = Box (hmap nat c) (hmap nat e)

-- wrong shape
-- instance HFunctor BoxF where
hmap2' :: (forall a. f a -> f' a) -> (forall a. g a -> g' a) -> BoxF f g c e -> BoxF f' g' c e
hmap2' natf natg (Box c e) = Box (hmap natf c) (hmap natg e)

instance (Monad g, Alternative g, Applicative f) => Semigroup (BoxF f g c e) where
  (<>) (Box c e) (Box c' e') = Box (c <> c') (e <> e')

instance (Monad g, Alternative g, Applicative f) => Monoid (BoxF f g c e) where
  mempty = Box mempty mempty
  mappend = (<>)

-- | a profunctor dimapMaybe
bmap :: (Monad m) => (a' -> m (Maybe a)) -> (b -> m (Maybe b')) -> Box m a b -> Box m a' b'
bmap fc fe (Box c e) = Box (mapC fc c) (mapE fe e)

-- instance (Functor f, Functor g) => Category (Box f g) where
--   id = Box ??? ???
-- (.) (Box c e) (Box c' e') = runIdentity $ glue c e' >> pure (Box c' e)

-- | composition of monadic boxes
dotb :: (Monad m) => Box m a b -> Box m b c -> m (Box m a c)
dotb (Box c e) (Box c' e') = glue c' e $> Box c e'

-- | composition of monadic boxes
dotf :: (Monad m) => Box m a b -> Box m b c -> m (Box m a c)
dotf (Box c e) (Box c' e') = glue c' e $> Box c e'

-- | composition of profunctors
-- dotp :: Box m a b -> Box m b c -> m (Box m a c)
-- dotp (Box c e) (Box c' e') = glue c' e $> Box c e'


-- | Connect an emitter directly to a committer of the same type.
--
-- The monadic action returns when the committer finishes.
glue :: (Monad m) => Committer m a -> Emitter m a -> m ()
glue c e = go
  where
    go = do
      a <- emit e
      c' <- maybe (pure False) (commit c) a
      when c' go

-- | Connect an emitter directly to a committer of the same type.
--
-- The monadic action returns if the committer returns False.
glue_ :: (Monad m) => Committer m a -> Emitter m a -> m ()
glue_ c e = go
  where
    go = do
      a <- emit e
      case a of
        Nothing -> go
        Just a' -> do
          b <- commit c a'
          bool (pure ()) go b

-- | Short-circuit a homophonuos box.
glueb :: (Monad m) => Box m a a -> m ()
glueb (Box c e) = glue c e

-- | fuse a box
--
-- > fuse (pure . pure) == glueb
fuse :: (Monad m) => (a -> m (Maybe b)) -> Box m b a -> m ()
fuse f (Box c e) = glue c (mapE f e)

-- | combines 'divide'/'conquer' and 'liftA2'/'pure'
class Divap p where
  divap :: (a -> (b, c)) -> ((d, e) -> f) -> p b d -> p c e -> p a f
  conpur :: a -> p b a

instance (Applicative f, Applicative g) => Divap (BoxF f g) where
  divap split' merge (Box lc le) (Box rc re) =
    Box (divide split' lc rc) (liftA2 (curry merge) le re)

  conpur a = Box conquer (pure a)

-- | combines 'Decidable' and 'Alternative'
class Profunctor p => DecAlt p where
  choice :: (a -> Either b c) -> (Either d e -> f) -> p b d -> p c e -> p a f
  loss :: p Void b

instance (Monad f, Functor g, Monad g, Alternative g) => DecAlt (BoxF f g) where
  choice split' merge (Box lc le) (Box rc re) =
    Box (choose split' lc rc) (fmap merge $ fmap Left le <|> fmap Right re)
  loss = Box (lose absurd) empty
