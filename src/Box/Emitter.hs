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
    mapE,
    readE,
    readE_,
    parseE,
    parseE_,
    premapE,
    postmapE,
    postmapM,
    toListE,
    unlistE,
    stateE,
    takeE,
    takeUntilE,
    filterE,
  )
where

import qualified Data.Attoparsec.Text as A
import qualified Data.Sequence as Seq
import Prelude
import Control.Monad.Morph
import Control.Monad.Trans.State.Lazy
import Control.Applicative
import qualified Data.Text as Text
import Data.Bool
import Data.Foldable
import Yaya.Functor
import Data.Maybe
import Data.Distributive
import Data.Functor.Compose
import Control.Monad.Trans.Maybe
import Data.Maybe

-- | an `Emitter` "emits" values of type a. A Source & a Producer (of a's) are the two other alternative but overloaded metaphors out there.
--
-- An Emitter "reaches into itself" for the value to emit, where itself is an opaque thing from the pov of usage.  An Emitter is named for its main action: it emits.
newtype Emitter g a = Emitter
  { emit :: Compose g Maybe a
  }

instance MFunctor Emitter where
  hoist nat (Emitter e) = Emitter (Compose $ nat (getCompose e))

instance HFunctor Emitter where
  hmap nat (Emitter e) = Emitter (Compose $ nat (getCompose e))

instance (Functor g) => Functor (Emitter g) where
  fmap f m = undefined -- Emitter $ Compose (fmap (fmap f) (emit m))

instance (Applicative g) => Applicative (Emitter g) where
  pure r = undefined -- Emitter (pure (pure r))

  mf <*> mx = undefined -- Emitter ((<*>) <$> emit mf <*> emit mx)

{-
instance (Monad g) => Monad (Emitter g) where
  return r = undefined -- Emitter (return (return r))

  m >>= g =
    Emitter $ do
      ma <- emit m
      case ma of
        Nothing -> return Nothing
        Just a -> emit (g a)

-}

instance (Alternative g) => Alternative (Emitter g) where
  empty = undefined -- Emitter (pure Nothing)

{-
    Emitter $
      let (i, ma) =
            fmap ((,) y) (emit x) <|> fmap ((,) x) (emit y) in
        _2 ma

-}
  x <|> y = undefined

{-
instance Distributive Maybe
  where
    distribute a = maybe (pure Nothing) (Just) a

-}

distribMaybe :: (Applicative g) => Compose Maybe g a -> Compose g Maybe a
distribMaybe = Compose . maybe (pure Nothing) (fmap Just) . getCompose

instance (Alternative g, Monad g) => Semigroup (Emitter g a) where
  (<>) = (<|>)

instance (Alternative g, Monad g) => Monoid (Emitter g a) where
  mempty = empty

  mappend = (<>)
{-

-}

-- | like a monadic mapMaybe. (See [witherable](https://hackage.haskell.org/package/witherable))
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

-- | parse emitter which returns the original text on failure
parseE :: (Functor m) => A.Parser a -> Emitter m String -> Emitter m (Either String a)
parseE parser e = (\t -> either (const $ Left t) Right (A.parseOnly parser (Text.pack t))) <$> e

-- | no error-reporting parsing
parseE_ :: (Monad m) => A.Parser a -> Emitter m String -> Emitter m a
parseE_ parser = mapE (pure . either (const Nothing) Just) . parseE parser

-- | read parse emitter, returning the original string on error
readE ::
  (Functor m, Read a) =>
  Emitter m String ->
  Emitter m (Either String a)
readE = fmap parsed
  where
    parsed str =
      case reads str of
        [(a, "")] -> Right a
        _err -> Left str

-- | no error-reporting reading
readE_ ::
  (Monad m, Read a) =>
  Emitter m String ->
  Emitter m a
readE_ = mapE (pure . either (const Nothing) Just) . readE

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

-- | add a post-emit monadic action on the emitted value (if there was any)
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
toListE e = go Seq.empty e
  where
    go xs e' = do
      x <- emit e'
      case x of
        Nothing -> pure (toList xs)
        Just x' -> go (xs Seq.:|> x') e'

-- | emit from a StateT Seq
--
-- FIXME: This compiles but is an infinite "a" emitter:
--
-- let e1 = hoist (flip evalStateT (Seq.fromList ["a", "b"::Text])) stateE :: Emitter IO Text
stateE :: (Monad m) => Emitter (StateT (Seq.Seq a) m) a
stateE = Emitter $ do
  xs' <- get
  case xs' of
    Seq.Empty -> pure Nothing
    (x Seq.:<| xs'') -> do
      put xs''
      pure $ Just x

-- | convert a list emitter to a Stateful element emitter
unlistE :: (Monad m) => Emitter m [a] -> Emitter (StateT [a] m) a
unlistE es = mapE unlistS (hoist lift es)
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
  x <- emit (hoist lift e)
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
filterE p e = Emitter go
  where
    go = do
      x <- emit e
      case x of
        Nothing -> pure Nothing
        Just x' ->
          bool go (pure (Just x')) (p x')
