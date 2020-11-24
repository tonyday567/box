{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | A box is something that commits and emits
module Box.Box
  ( Box (..),
    bmap,
    hoistb,
    glue,
    glue_,
    glueb,
    fuse,
    dotb,
    Divap(..),
    DecAlt(..),
  )
where

import Box.Committer
import Box.Emitter
import Data.Functor.Contravariant
import Data.Profunctor
import NumHask.Prelude
import Data.Functor.Contravariant.Divisible

-- | A Box is a product of a Committer m and an Emitter. Think of a box with an incoming wire and an outgoing wire. Now notice that the abstraction is reversable: are you looking at two wires from "inside a box"; a blind erlang grunt communicating with the outside world via the two thin wires, or are you looking from "outside the box"; interacting with a black box object. Either way, it's a box.
-- And either way, the committer is contravariant and the emitter covariant so it forms a profunctor.
--
-- a Box can also be seen as having an input tape and output tape, thus available for turing and finite-state machine metaphorics.
data Box m c e
  = Box
      { committer :: Committer m c,
        emitter :: Emitter m e
      }

-- | Wrong signature for the MFunctor class
hoistb :: Monad m => (forall a. m a -> n a) -> Box m c e -> Box n c e
hoistb nat (Box c e) = Box (hoist nat c) (hoist nat e)

instance (Functor m) => Profunctor (Box m) where
  dimap f g (Box c e) = Box (contramap f c) (fmap g e)

instance (Alternative m, Monad m) => Semigroup (Box m c e) where
  (<>) (Box c e) (Box c' e') = Box (c <> c') (e <> e')

instance (Alternative m, Monad m) => Monoid (Box m c e) where
  mempty = Box mempty mempty
  mappend = (<>)

-- | a profunctor dimapMaybe
bmap :: (Monad m) => (a' -> m (Maybe a)) -> (b -> m (Maybe b')) -> Box m a b -> Box m a' b'
bmap fc fe (Box c e) = Box (mapC fc c) (mapE fe e)

{-
instance Category (Box Identity) where
  id = Box ??? ???
  (.) (Box c e) (Box c' e') = runIdentity $ glue c e' >> pure (Box c' e)
-}

-- | composition of monadic boxes
dotb :: (Monad m) => Box m a b -> Box m b c -> m (Box m a c)
dotb (Box c e) (Box c' e') = glue c' e *> pure (Box c e')

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
          case b of
            True -> go
            False -> pure ()

-- | Short-circuit a homophonuos box.
glueb :: (Monad m) => Box m a a -> m ()
glueb (Box c e) = glue c e

-- | fuse a box
--
-- > fuse (pure . pure) == glueb 
fuse :: (Monad m) => (a -> m (Maybe b)) -> Box m b a -> m ()
fuse f (Box c e) = glue c (mapE f e)

-- combines 'divide'/'conquer' and 'liftA2'/'pure'
class Divap p where
    divap :: (a -> (b, c)) -> ((d, e) -> f) -> p b d -> p c e -> p a f
    conpur :: a -> p b a

class Profunctor p => DecAlt p where
    choice :: (a -> Either b c) -> (Either d e -> f) -> p b d -> p c e -> p a f
    loss :: p Void b

instance (Applicative m) => Divap (Box m) where
  divap split' merge (Box lc le) (Box rc re) = Box (divide split' lc rc) (liftA2 (curry merge) le re)

  conpur a = Box conquer (pure a)

instance (Monad m, Alternative m) => DecAlt (Box m) where
  choice split merge (Box lc le) (Box rc re) =
    Box (choose split lc rc) (fmap merge $ (fmap Left le) <|> (fmap Right re))
  loss = Box (lose absurd) empty




