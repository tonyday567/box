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
-- This library is a combination of ideas and code from [pipes-concurrency](https://hackage.haskell.org/package/pipes-concurrency) and [mvc](https://hackage.haskell.org/package/mvc) but with pretentious names.
--
--
-- https://www.schoolofhaskell.com/school/advanced-haskell/beautiful-concurrency/3-software-transactional-memory
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
-- >>> let cDelay = maybeCommit (\b -> sleep 0.1 >> pure (Just b)) <$> (liftC <$> cStdout 100)
-- >>> let cImmediate = liftC <$> cStdout 100
--
-- The monoid instance sends each commit to both mappended committers
--
-- >>> (etcM () transducer' $ (Box <$> (cImmediate <> cDelay) <*> (liftE <$> emitter'))) >> sleep 1
-- echo: hi
-- echo: hi
-- echo: bye
-- echo: bye
--
-- >>> let c = fmap liftC $ cStdout 10
-- >>> let e = fmap liftE $ toEmit (S.each ["hi","bye","q","x"])
-- >>> let c' = maybeCommit (\a -> if a=="q" then (sleep 1 >> putStrLn "stolen!" >> sleep 1 >> pure (Nothing)) else (pure (Just a))) <$> c :: Cont IO (Committer IO Text)
-- >>> fuse (pure . pure) $ Box <$> c' <*> e
-- hi
-- bye
-- stolen!
-- x
--

-- $emit
--
-- >>> with (S.each [0..] & toEmit) (atomically . emit) >>= print
-- Just 0
--
-- >>> let c = fmap liftC $ cStdout 10
-- >>> let e = fmap liftE $ toEmit (S.each ["hi","bye","q","x"])
-- >>> let e' = maybeEmit (\a -> if a=="q" then (sleep 0.1 >> putStrLn "stole a q!" >> sleep 0.1 >> pure (Nothing)) else (pure (Just a))) <$> e :: Cont IO (Emitter IO Text)
-- >>> fuse (pure . pure) $ Box <$> c <*> e'
-- hi
-- bye
-- stole a q!
-- x
--
-- >>> let e1 = fmap show <$> (toEmit $ delayTimed (S.each (zip (fromIntegral <$> [1..10]) ['a'..]))) :: Cont IO (Emitter STM Text)
-- >>> let e2 = fmap show <$> (toEmit $ delayTimed (S.each (zip ((\x -> fromIntegral x + 0.1) <$> [1..10]) (reverse ['a'..'z'])))) :: Cont IO (Emitter STM Text)
-- >>> let b = Box <$> cStdout 6 <*> mergeEmit ((,) <$> e1 <*> e2)
-- >>> etc () (Transducer identity) b
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

