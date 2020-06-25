{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | `emit`
module Box.Emitter
  ( Emitter (..),
    liftE,
    emap,
    keeps,
    eRead,
    eParse,
  )
where

import Prelude
import Control.Applicative
import Control.Monad
import Control.Monad.Conc.Class as C
import qualified Data.Attoparsec.Text as A
import Data.Functor.Constant
import Data.Monoid
import qualified Data.Text as Text
import Data.Text (Text)
import Control.Monad.Morph

-- | an `Emitter` "emits" values of type a. A Source & a Producer (of a's) are the two other alternative but overloaded metaphors out there.
--
-- An Emitter "reaches into itself" for the value to emit, where itself is an opaque thing from the pov of usage.  An Emitter is named for its main action: it emits.
newtype Emitter m a
  = Emitter
      { emit :: m (Maybe a)
      }

instance MFunctor Emitter where
  hoist nat (Emitter e) = Emitter (nat e)

instance (Functor m) => Functor (Emitter m) where
  fmap f m = Emitter (fmap (fmap f) (emit m))

instance (Applicative m) => Applicative (Emitter m) where

  pure r = Emitter (pure (pure r))

  mf <*> mx = Emitter ((<*>) <$> emit mf <*> emit mx)

instance (Monad m) => Monad (Emitter m) where

  return r = Emitter (return (return r))

  m >>= f =
    Emitter $ do
      ma <- emit m
      case ma of
        Nothing -> return Nothing
        Just a -> emit (f a)

instance (Monad m, Alternative m) => Alternative (Emitter m) where

  empty = Emitter (pure Nothing)

  x <|> y =
    Emitter $ do
      (i, ma) <- fmap ((,) y) (emit x) <|> fmap ((,) x) (emit y)
      case ma of
        Nothing -> emit i
        Just a -> pure (Just a)

instance (Alternative m, Monad m) => MonadPlus (Emitter m) where

  mzero = empty

  mplus = (<|>)

instance (Alternative m, Monad m) => Semigroup (Emitter m a) where
  (<>) = (<|>)

instance (Alternative m, Monad m) => Monoid (Emitter m a) where

  mempty = empty

  mappend = (<>)

-- | lift an STM emitter
liftE :: (MonadConc m) => Emitter (STM m) a -> Emitter m a
liftE = hoist atomically

-- | like a monadic mapMaybe. (See [witherable](https://hackage.haskell.org/package/witherable))
emap :: (Monad m) => (a -> m (Maybe b)) -> Emitter m a -> Emitter m b
emap f e = Emitter go
  where
    go = do
      a <- emit e
      case a of
        Nothing -> pure Nothing
        Just a' -> do
          fa <- f a'
          case fa of
            Nothing -> go
            Just fa' -> pure (Just fa')

-- | prism handler
keeps ::
  (Monad m) =>
  -- |
  ((b -> Constant (First b) b) -> (a -> Constant (First b) a)) ->
  -- |
  Emitter m a ->
  Emitter m b
keeps k (Emitter emit_) = Emitter emit_'
  where
    emit_' = do
      ma <- emit_
      case ma of
        Nothing -> return Nothing
        Just a ->
          case match a of
            Nothing -> emit_'
            Just b -> return (Just b)
    match = getFirst . getConstant . k (Constant . First . Just)

-- | parse emitter which returns the original text on failure
eParse :: (Functor m) => A.Parser a -> Emitter m Text -> Emitter m (Either Text a)
eParse parser e = (\t -> either (const $ Left t) Right (A.parseOnly parser t)) <$> e

-- | read parse emitter
eRead ::
  (Functor m, Read a) =>
  Emitter m Text ->
  Emitter m (Either Text a)
eRead = fmap $ parsed . Text.unpack
  where
    parsed str =
      case reads str of
        [(a, "")] -> Right a
        _ -> Left (Text.pack str)
