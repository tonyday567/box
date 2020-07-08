{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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
    emap,
    keeps,
    eRead,
    eRead_,
    eParse,
    eParse_,
    premapE,
    postmapE,
    postmapM,
    toListE,
    unlistE,
    stateE,
  )
where

import qualified Data.Attoparsec.Text as A
import NumHask.Prelude

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

-- | no error-reporting parsing
eParse_ :: (Monad m) => A.Parser a -> Emitter m Text -> Emitter m a
eParse_ parser = emap (pure . (either (const Nothing) Just)) . eParse parser

-- | read parse emitter, returning the original string on error
eRead ::
  (Functor m, Read a) =>
  Emitter m Text ->
  Emitter m (Either Text a)
eRead = fmap $ parsed . unpack
  where
    parsed str =
      case reads str of
        [(a, "")] -> Right a
        _ -> Left (pack str)

-- | no error-reporting reading
eRead_ ::
  (Monad m, Read a) =>
  Emitter m Text ->
  Emitter m a
eRead_ = emap (pure . (either (const Nothing) Just)) . eRead

-- | adds a pre-emit monadic action to the emitter
premapE ::
  (Applicative m) =>
  (Emitter m a -> m ()) ->
  Emitter m a ->
  Emitter m a
premapE f e = Emitter $ f e *> emit e

-- | adds a post-emit monadic action to the emitter
postmapE ::
  (Monad m) =>
  (Emitter m a -> m ()) ->
  Emitter m a ->
  Emitter m a
postmapE f e = Emitter $ do
  r <- emit e
  f e
  pure r

-- | add a post-emit monadic action
postmapM ::
  (Monad m) =>
  (a -> m ()) ->
  Emitter m a ->
  Emitter m a
postmapM f e = Emitter $ do
  r <- emit e
  case r of
    Nothing -> pure Nothing
    Just r' -> do
      f r'
      pure (Just r')

-- | turn an emitter into a list
toListE :: (Monad m) => Emitter m a -> m [a]
toListE e = go [] e
  where
    go xs e' = do
      x <- emit e'
      case x of
        Nothing -> pure (reverse xs)
        Just x' -> go (x' : xs) e'

-- | emit from a StateT list
--
-- This compiles but is an infinite "a" emitter:
--
-- let e1 = hoist (flip evalStateT ["a", "b"::Text]) stateE :: Emitter IO Text
stateE :: (Monad m) => Emitter (StateT [a] m) a
stateE = Emitter $ do
  xs' <- get
  case xs' of
    [] -> pure Nothing
    (x : xs'') -> do
      put xs''
      pure $ Just x

-- | convert a list emitter to a Stateful element emitter
unlistE :: (Monad m) => Emitter m [a] -> Emitter (StateT [a] m) a
unlistE es = emap unlistS (hoist lift es)
  where
    unlistS xs = do
      rs <- get
      case rs <> xs of
        [] -> pure Nothing
        (x : xs') -> do
          put xs'
          pure (Just x)
