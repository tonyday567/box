{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

--

module Box.Functor
  ( FFunctor (..),
    FoldableM (..),
  )
where

import Data.Kind
import Prelude

-- | An endofunctor in the category of endofunctors.
--
--  Like `Control.Monad.Morph.MFunctor` but without a `Monad` constraint.
class FFunctor (h :: (Type -> Type) -> Type -> Type) where
  foist :: (forall x. f x -> g x) -> h f a -> h g a

-- | Monadically Foldable
class FoldableM (t :: (Type -> Type) -> Type -> Type) where
  foldrM :: (Monad m) => (a -> m b -> m b) -> m b -> t m a -> m b
