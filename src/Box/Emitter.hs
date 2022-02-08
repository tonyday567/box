{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | emit
module Box.Emitter
  ( Emitter (..),
    type CoEmitter,
    toListM,
    mapE,
    readE,
    parseE,
    unlistE,
    takeE,
    takeUntilE,
    filterE,
    pop,
  )
where

import Control.Applicative
import Control.Monad.State.Lazy
import qualified Data.Attoparsec.Text as A
import Data.Bool
import Data.Text (Text, pack, unpack)
import Prelude
import Box.Cont (Codensity)
import Box.Functor
import qualified Data.Sequence as Seq
import qualified Data.DList as D

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Box
-- >>> import Prelude
-- >>> import Data.Bool

-- | an `Emitter` "emits" values of type a. A Source & a Producer (of a's) are the two other alternative but overloaded metaphors out there.
--
-- An Emitter "reaches into itself" for the value to emit, where itself is an opaque thing from the pov of usage.  An Emitter is named for its main action: it emits.
-- >>> e = Emitter (pure (Just "I'm emitted"))
-- >>> emit e
-- Just "I'm emitted"
--
-- >>> emit mempty
-- Nothing
newtype Emitter m a = Emitter
  { emit :: m (Maybe a)
  }

-- | CPS version of an emitter
type CoEmitter m a = Codensity m (Emitter m a)

instance FFunctor Emitter where
  foist nat (Emitter e) = Emitter (nat e)

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

  -- | Zero or more.
  many e = Emitter $ Just <$> toListM e

instance (Alternative m, Monad m) => MonadPlus (Emitter m) where
  mzero = empty

  mplus = (<|>)

instance (Alternative m, Monad m) => Semigroup (Emitter m a) where
  (<>) = (<|>)

instance (Alternative m, Monad m) => Monoid (Emitter m a) where
  mempty = empty

  mappend = (<>)


-- | This fold completes on the first Nothing emitted, which may not be what you want.
instance FoldableM Emitter where
  foldrM acc begin e =
    maybe begin (\a' -> foldrM acc (acc a' begin) e) =<< emit e

-- | Collect emitter emits into a list, on the first Nothing emitted.
toListM :: (Monad m) => Emitter m a -> m [a]
toListM e = D.toList <$> foldrM (\a acc -> fmap (`D.snoc` a) acc) (pure D.empty) e

-- | like a monadic mapMaybe. (See [witherable](https://hackage.haskell.org/package/witherable))
--
-- >>> close $ toListM <$> mapE (\x -> bool (print x >> pure Nothing) (pure (Just x)) (even x)) <$> (qList [0..4])
-- 1
-- 3
-- [0,2,4]
mapE :: (Monad m) => (a -> m (Maybe b)) -> Emitter m a -> Emitter m b
mapE f e = Emitter go
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

-- | attoparsec parse emitter which returns the original text on failure
parseE :: (Functor m) => A.Parser a -> Emitter m Text -> Emitter m (Either Text a)
parseE parser e = (\t -> either (const $ Left t) Right (A.parseOnly parser t)) <$> e

-- | read parse emitter, returning the original string on error
readE ::
  (Functor m, Read a) =>
  Emitter m Text ->
  Emitter m (Either Text a)
readE = fmap $ parsed . unpack
  where
    parsed str =
      case reads str of
        [(a, "")] -> Right a
        _err -> Left (pack str)

-- | convert a list emitter to a Stateful element emitter
unlistE :: (Monad m) => Emitter m [a] -> Emitter (StateT [a] m) a
unlistE es = mapE unlistS (foist lift es)
  where
    unlistS xs = do
      rs <- get
      case rs <> xs of
        [] -> pure Nothing
        (x : xs') -> do
          put xs'
          pure (Just x)

-- | Stop an 'Emitter' after n 'emit's
takeE :: (Monad m) => Int -> Emitter m a -> Emitter (StateT Int m) a
takeE n e = Emitter $ do
  x <- emit (foist lift e)
  case x of
    Nothing -> pure Nothing
    Just x' -> do
      n' <- get
      bool (pure Nothing) (emit' n') (n' < n)
      where
        emit' n' = do
          put (n' + 1)
          pure $ Just x'

-- | Take from an emitter until predicate
takeUntilE :: (Monad m) => (a -> Bool) -> Emitter m a -> Emitter m a
takeUntilE p e = Emitter $ do
  x <- emit e
  case x of
    Nothing -> pure Nothing
    Just x' ->
      bool (pure (Just x')) (pure Nothing) (p x')

-- | Filter emissions according to a predicate.
filterE :: (Monad m) => (a -> Bool) -> Emitter m a -> Emitter m a
filterE p = mapE (\a -> bool (pure (Just a)) (pure Nothing) (p a))

-- | pop from a StateT Seq
pop :: (Monad m) => Emitter (StateT (Seq.Seq a) m) a
pop = Emitter $ do
  xs <- get
  case xs of
    Seq.Empty -> pure Nothing
    (x Seq.:<| xs') -> do
      put xs'
      pure (Just x)
