{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
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
import NumHask.Prelude

-- | an `Emitter` "emits" values of type a. A Source & a Producer (of a's) are the two other alternative but overloaded metaphors out there.
--
-- An Emitter "reaches into itself" for the value to emit, where itself is an opaque thing from the pov of usage.  An Emitter is named for its main action: it emits.
newtype Emitter m a = Emitter
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
parseE :: (Functor m) => A.Parser a -> Emitter m Text -> Emitter m (Either Text a)
parseE parser e = (\t -> either (const $ Left t) Right (A.parseOnly parser t)) <$> e

-- | no error-reporting parsing
parseE_ :: (Monad m) => A.Parser a -> Emitter m Text -> Emitter m a
parseE_ parser = mapE (pure . either (const Nothing) Just) . parseE parser

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

-- | no error-reporting reading
readE_ ::
  (Monad m, Read a) =>
  Emitter m Text ->
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
