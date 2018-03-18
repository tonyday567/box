{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

-- | Boxes that `emit`, `transduce` & `commit`
--
-- This library follows the ideas and code from [pipes-concurrency](https://hackage.haskell.org/package/pipes-concurrency) and [mvc](https://hackage.haskell.org/package/mvc) but with some polymorphic tweaks and definitively more pretentious names.
--
--
module Box
  ( -- $setup
    -- $commit
    -- $emit
    -- $transduce
    module Box.Box
  , module Box.Committer
  , module Box.Connectors
  , module Box.Cont
  , module Box.Emitter
  , module Box.IO
  , module Box.Plugs
  , module Box.Queue
  , module Box.Stream
  , module Box.Time
  , module Box.Transducer
  ) where

import Box.Box
import Box.Committer
import Box.Connectors
import Box.Cont
import Box.Emitter
import Box.IO
import Box.Plugs
import Box.Queue
import Box.Stream
import Box.Time
import Box.Transducer

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XGADTs
-- >>> import Protolude
-- >>> import Box
-- >>> import qualified Streaming.Prelude as S
-- >>> import Control.Monad.Conc.Class as C
-- >>> let committer' = cStdout 100
-- >>> let emitter' = toEmit (S.each ["hi","bye","q","x"])
-- >>> let box' = Box <$> committer' <*> emitter'
-- >>> let transducer' = Transducer $ \s -> s & S.takeWhile (/="q") & S.map ("echo: " <>)

-- $commit
-- committing
--
-- >>> with (cStdout 100) $ \c -> C.atomically (commit c "something")
-- something
-- True
--
-- The monoid instance sends each commit to both mappended committers. Delaying effects are introduced to these examples to keep stdout clear of race effects.
--
-- turned off in doctest until the muddled upedness is sorted...
-- > let cDelay = cmap (\b -> sleep 0.1 >> pure (Just b)) <$> (liftC <$> cStdout 100)
-- > let cImmediate = liftC <$> cStdout 100
-- > (etcM () transducer' $ (Box <$> (cImmediate <> cDelay) <*> (liftE <$> emitter'))) >> sleep 1
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

-- | splitCommit
-- 
-- >>> cs <- splitCommit (cStdout 100)
-- >>> let c2 = contCommit <$> cs
-- >>> (etcM () transducer' $ (Box <$> c2 <*> (liftE <$> emitter'))) >> sleep 1
--

-- $emit
--
-- >>> with (S.each [0..] & toEmit) (C.atomically . emit) >>= print
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
-- >>> let e1 = fmap show <$> (toEmit $ delayTimed (S.each (zip (fromIntegral <$> [1..10]) ['a'..]))) :: Cont IO (Emitter (C.STM IO) Text)
-- >>> let e2 = fmap show <$> (toEmit $ delayTimed (S.each (zip ((\x -> fromIntegral x + 0.1) <$> [1..10]) (reverse ['a'..'z'])))) :: Cont IO (Emitter (C.STM IO) Text)
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

-- | broadcasting
--
-- > (bcast, bcom) <- C.atomically broadcast
-- > (funn, fem) <- C.atomically funnel
-- >

