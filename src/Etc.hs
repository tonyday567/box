{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

-- | `emit` - `transduce` - `commit`
--
-- This library follows the ideas and code from [pipes-concurrency](https://hackage.haskell.org/package/pipes-concurrency) and [mvc](https://hackage.haskell.org/package/mvc) but with some polymorphic tweaks and definitively more pretentious names.
--
--
module Etc
  ( -- $setup
    -- $commit
    -- $emit
    -- $transduce
    module Etc.Box
  , module Etc.Committer
  , module Etc.Connectors
  , module Etc.Cont
  , module Etc.Emitter
  , module Etc.IO
  , module Etc.Plugs
  , module Etc.Queue
  , module Etc.Stream
  , module Etc.Time
  , module Etc.Transducer
  ) where

import Etc.Box
import Etc.Committer
import Etc.Connectors
import Etc.Cont
import Etc.Emitter
import Etc.IO
import Etc.Plugs
import Etc.Queue
import Etc.Stream
import Etc.Time
import Etc.Transducer

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Protolude
-- >>> import Etc
-- >>> import qualified Streaming.Prelude as S
-- >>> let committer' = cStdout 100
-- >>> let emitter' = toEmit (S.each ["hi","bye","q","x"])
-- >>> let box' = Box <$> committer' <*> emitter'
-- >>> let transducer' = Transducer $ \s -> s & S.takeWhile (/="q") & S.map ("echo: " <>)

-- $commit
-- committing
--
-- >>> with (cStdout 100) $ \c -> atomically (commit c "something")
-- something
-- True
--
-- The monoid instance sends each commit to both mappended committers. Delaying effects are introduced to these examples to keep stdout clear of race effects.
--
-- >>> let cDelay = cmap (\b -> sleep 0.1 >> pure (Just b)) <$> (liftC <$> cStdout 100)
-- >>> let cImmediate = liftC <$> cStdout 100
-- >>> (etcM () transducer' $ (Box <$> (cImmediate <> cDelay) <*> (liftE <$> emitter'))) >> sleep 1
-- echo: hi
-- echo: hi
-- echo: bye
-- echo: bye
--
-- >>> let c = fmap liftC $ cStdout 10
-- >>> let e = fmap liftE $ toEmit (S.each ["hi","bye","q","x"])
-- >>> let c' = cmap (\a -> if a=="q" then (sleep 1 >> putStrLn "stolen!" >> sleep 1 >> pure (Nothing)) else (pure (Just a))) <$> c :: Cont IO (Committer IO Text)
-- >>> fuse (pure . pure) $ Box <$> c' <*> e
-- hi
-- bye
-- stolen!
-- x
--
-- prism handler
--
-- >>> import Control.Lens (_Right)
-- >>> let e2 = (fmap (\x -> Right (x <> "_right")) <$> e) <> (fmap (\x -> Left (x <> "_left")) <$> e)
-- >>> let cright = handles _Right <$> c
-- >>> fuse (pure . pure) $ Box <$> cright <*> e2
-- hi_right
-- bye_right
-- q_right
-- x_right
--

-- $emit
--
-- >>> with (S.each [0..] & toEmit) (atomically . emit) >>= print
-- Just 0
--
-- >>> let c = fmap liftC $ cStdout 10
-- >>> let e = fmap liftE $ toEmit (S.each ["hi","bye","q","x"])
-- >>> let e' = emap (\a -> if a=="q" then (sleep 0.1 >> putStrLn "stole a q!" >> sleep 0.1 >> pure (Nothing)) else (pure (Just a))) <$> e :: Cont IO (Emitter IO Text)
-- >>> fuse (pure . pure) $ Box <$> c <*> e'
-- hi
-- bye
-- stole a q!
-- x
--
-- >>> let e1 = fmap show <$> (toEmit $ delayTimed (S.each (zip (fromIntegral <$> [1..10]) ['a'..]))) :: Cont IO (Emitter STM Text)
-- >>> let e2 = fmap show <$> (toEmit $ delayTimed (S.each (zip ((\x -> fromIntegral x + 0.1) <$> [1..10]) (reverse ['a'..'z'])))) :: Cont IO (Emitter STM Text)
-- >>> let e12 = e1 <> e2
-- >>> etc () (Transducer identity) $ Box <$> cStdout 6 <*> emerge ((,) <$> e1 <*> e2)
-- 'a'
-- 'z'
-- 'b'
-- 'y'
-- 'c'
-- 'x'
--
-- >>> etc () (Transducer identity) $ Box <$> cStdout 6 <*> (liftA2 (<>) e1 e2)
-- 'a'
-- 'z'
-- 'b'
-- 'y'
-- 'c'
-- 'x'
--

-- $transduce
--
-- >>> etc () transducer' box'
-- echo: hi
-- echo: bye
--

