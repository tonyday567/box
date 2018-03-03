{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | `emit`
module Etc.Emitter
  ( Emitter(..)
  , maybeEmit
  , liftE
  , keeps
  , eRead
  , eRead'
  , eParse
  ) where

import Control.Category ((.))
import Control.Lens hiding ((:>), (.>), (<|), (|>))
import Data.Functor.Constant
import Data.Semigroup hiding (First, getFirst)
import Etc.Cont
import Protolude hiding ((.), (<>))
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as Text

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import Etc
-- >>> import Etc.Time
-- >>> import qualified Streaming.Prelude as S
-- >>> let committer' = cStdout 100 unbounded
-- >>> let emitter' = toEmit (bounded 1) (S.each ["hi","bye","q","x"])
-- >>> let box' = Box <$> committer' <*> emitter'
-- >>> let transducer' = Transducer $ \s -> s & S.takeWhile (/="q") & S.map ("echo: " <>)
--
-- | an Emitter emits values of type a. A Source of 'a's & a Producer of 'a's are the two obvious alternative but overloaded metaphors out there. It is a newtype warpper around pipes-concurrency Input to pick up the instances.
--
-- Naming is reversed in comparison to the 'Input' it wraps.  An Emitter 'reaches into itself' for the value to emit, where itself is an opaque thing from the pov of usage.  An Emitter is named for its' main action: it emits.
--
-- >>> with (S.each [0..] & toEmit Unbounded) (atomically . emit) >>= print
-- Just 0
--
newtype Emitter m a = Emitter
  { emit :: m (Maybe a)
  }

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
        Just a -> return (Just a)

instance (Alternative m, Monad m) => MonadPlus (Emitter m) where
  mzero = empty
  mplus = (<|>)

instance (Alternative m, Monad m) => Semigroup (Emitter m a) where
  (<>) = (<|>)

instance (Alternative m, Monad m) => Monoid (Emitter m a) where
  mempty = empty
  mappend = (<>)

-- | lift an emitter from STM to IO
liftE :: Emitter STM a -> Emitter IO a
liftE = Emitter . atomically . emit

-- | maybe emit based on an action
--
-- >>> let c = fmap liftC $ cStdout 10 unbounded
-- >>> let e = fmap liftE $ toEmit (bounded 1) (S.each ["hi","bye","q","x"])
-- >>> let e' = maybeEmit (\a -> if a=="q" then (sleep 0.1 >> putStrLn "stole a q!" >> sleep 0.1 >> pure (Nothing)) else (pure (Just a))) <$> e :: Managed IO (Emitter IO Text)
-- >>> fuse (pure . pure) $ Box <$> c <*> e'
-- hi
-- bye
-- stole a q!
-- x
--
maybeEmit :: (Monad m) => (a -> m (Maybe b)) -> Emitter m a -> Emitter m b
maybeEmit f e = Emitter go
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

-- | keep valid emissions
keeps ::
     (Monad m)
  => ((b -> Constant (First b) b) -> (a -> Constant (First b) a))
    -- ^
  -> Emitter m a
    -- ^
  -> Emitter m b
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

-- | parsing, throwing away errors
eParse :: A.Parser a -> Cont IO (Emitter STM Text) -> Cont IO (Emitter STM a)
eParse parser e = keeps _Right . fmap (A.parseOnly parser) <$> e

-- | read an a, throwing away errors
eRead :: (Read a) => Cont IO (Emitter STM Text) -> Cont IO (Emitter STM a)
eRead = fmap (keeps parsed) . fmap (fmap Text.unpack)
  where
    parsed k str =
      case reads str of
        [(a, "")] -> Constant (getConstant (k a))
        _ -> pure str

eRead' ::
     (Read a)
  => Cont IO (Emitter STM Text)
  -> Cont IO (Emitter STM (Either Text a))
eRead' = fmap (fmap $ parsed . Text.unpack)
  where
    parsed str =
      case reads str of
        [(a, "")] -> Right a
        _ -> Left (Text.pack str)
