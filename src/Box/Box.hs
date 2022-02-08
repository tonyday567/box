{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | A box is something that commits and emits
module Box.Box
  ( Box (..),
    CoBox,
    CoBoxM (..),
    bmap,
    foistb,
    glue,
    fuse,
    Divap (..),
    DecAlt (..),
    cobox,
    seqBox,
  )
where

import Box.Committer
import Box.Emitter
import Control.Applicative
  ( Alternative (empty, (<|>)),
    Applicative (liftA2),
  )
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Functor.Contravariant.Divisible
  ( Decidable (choose, lose),
    Divisible (conquer, divide),
  )
import Data.Profunctor (Profunctor (dimap))
import Data.Void (Void, absurd)
import Prelude hiding ((.), id)
import Box.Cont
import Control.Monad.Codensity
import Control.Monad.State.Lazy
import qualified Data.Sequence as Seq
import Data.Bool
import Data.Semigroupoid
import Box.Functor

-- | A Box is a product of a Committer m and an Emitter. Think of a box with an incoming wire and an outgoing wire. Now notice that the abstraction is reversable: are you looking at two wires from "inside a box"; a blind erlang grunt communicating with the outside world via the two thin wires, or are you looking from "outside the box"; interacting with a black box object. Either way, it's a box.
-- And either way, the committer is contravariant and the emitter covariant so it forms a profunctor.
--
data Box m c e = Box
  { committer :: Committer m c,
    emitter :: Emitter m e
  }

-- | Wrong kind signature for the FFunctor class
foistb :: (forall a. m a -> n a) -> Box m c e -> Box n c e
foistb nat (Box c e) = Box (foist nat c) (foist nat e)

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

-- | Connect an emitter directly to a committer of the same type.
--
-- The monadic action returns when the committer finishes.
glue :: (Monad m) => Committer m a -> Emitter m a -> m ()
glue c e = fix $ \rec -> emit e >>= maybe (pure False) (commit c) >>= bool (pure ()) rec

-- | fuse a box
--
-- > fuse (pure . pure) == \(Box c e) -> glue c e
fuse :: (Monad m) => (a -> m (Maybe b)) -> Box m b a -> m ()
fuse f (Box c e) = glue c (mapE f e)

-- | combines 'divide'/'conquer' and 'liftA2'/'pure'
class Divap p where
  divap :: (a -> (b, c)) -> ((d, e) -> f) -> p b d -> p c e -> p a f
  conpur :: a -> p b a

instance (Applicative m) => Divap (Box m) where
  divap split' merge (Box lc le) (Box rc re) =
    Box (divide split' lc rc) (liftA2 (curry merge) le re)

  conpur a = Box conquer (pure a)

-- | combines 'Decidable' and 'Alternative'
class Profunctor p => DecAlt p where
  choice :: (a -> Either b c) -> (Either d e -> f) -> p b d -> p c e -> p a f
  loss :: p Void b

instance (Monad m, Alternative m) => DecAlt (Box m) where
  choice split' merge (Box lc le) (Box rc re) =
    Box (choose split' lc rc) (fmap merge $ fmap Left le <|> fmap Right re)
  loss = Box (lose absurd) empty

type CoBox m a b = Codensity m (Box m a b)

cobox :: CoCommitter m a -> CoEmitter m b -> CoBox m a b
cobox c e = Box <$> c <*> e

seqBox :: (Monad m) => Box (StateT (Seq.Seq a) m) a a
seqBox = Box push pop

-- | cps composition of monadic boxes
dotco :: Monad m => Codensity m (Box m a b) -> Codensity m (Box m b c) -> Codensity m (Box m a c)
dotco b b' = lift $ do
  (Box c e) <- lowerCodensity b
  (Box c' e') <- lowerCodensity b'
  glue c' e
  pure (Box c e')

newtype CoBoxM m a b = CoBoxM { uncobox :: Codensity m (Box m a b) }

instance (Monad m) => Semigroupoid (CoBoxM m) where
  o (CoBoxM b) (CoBoxM b')= CoBoxM (dotco b' b)
