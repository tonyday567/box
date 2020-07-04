{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

-- | Boxes that `emit`, `transduce` & `commit`
--
-- This library follows the ideas and code from [pipes-concurrency](https://hackage.haskell.org/package/pipes-concurrency) and [mvc](https://hackage.haskell.org/package/mvc) but with some polymorphic tweaks and definitively more pretentious names.
module Box
  ( -- $setup
    -- $commit
    -- $emit
    -- $transduce
    module Box.Box,
    module Box.Broadcast,
    module Box.Committer,
    module Box.Connectors,
    module Box.Cont,
    module Box.Emitter,
    module Box.IO,
    module Box.Plugs,
    module Box.Queue,
    module Box.Stream,
    module Box.Time,
    module Box.Transducer,
  )
where

import Box.Box
import Box.Broadcast
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
-- >>> import Data.Functor.Contravariant
-- >>> import Box
-- >>> import qualified Streaming.Prelude as S
-- >>> import Control.Monad.Conc.Class as C
-- >>> import Data.Text (Text)
-- >>> import qualified Data.Text as Text
-- >>> import Control.Applicative
-- >>> import Control.Lens ((&))
-- >>> let committer' = cStdout 100
-- >>> let emitter' = toEmit (S.each ["hi","bye","q","x"])
-- >>> let box' = Box <$> committer' <*> emitter'
-- >>> let transducer' = Transducer $ \s -> s & S.takeWhile (/="q") & S.map ("echo: " <>)

-- | emitting
-- > flip (with . fmap toListE . fromListE) id == pure
-- >>> with (fromListE [1..3]) toListE
-- [1,2,3]
--
-- > fuse (pure . Just) $ Box <$> (liftC <$> showStdout) <*> (fromListE [1..3::Int])

-- $commit
-- committing
--
-- >>> with (cStdout 100) $ \c -> C.atomically (commit c "something")
-- something
-- True
--
-- The monoid instance sends each commit to both mappended committers. Because everything is concurrent, race effects are common on stdout, so we introduce some delaying effects to (hopefully) avoid races.
--
-- >>> let cFast = cmap (\b -> sleep 0.01 >> pure (Just b)) . liftC . contramap ("fast: " <>) <$> (cStdout 100)
-- >>> let cSlow = cmap (\b -> sleep 0.1 >> pure (Just b)) . liftC . contramap ("slow: " <>) <$> (cStdout 100)
-- >>> (etcM () transducer' $ (Box <$> (cFast <> cSlow) <*> (liftE <$> emitter'))) >> sleep 1
-- fast: echo: hi
-- slow: echo: hi
-- fast: echo: bye
-- slow: echo: bye
--
-- >>> let c = fmap liftC $ cStdout 10
-- >>> let e = fmap liftE $ toEmit (S.each ["hi","bye","q","x"])
-- >>> let c' = cmap (\a -> if a=="q" then (sleep 0.1 >> putStrLn "stolen!" >> sleep 0.1 >> pure (Nothing)) else (pure (Just a))) <$> c :: Cont IO (Committer IO Text)
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
-- Splits a committer into two.
-- >>> let cs = splitCommit (liftC <$> cStdout 100)
-- >>> let cc = contCommit <$> cs <*> pure (cmap (\b -> sleep 0.01 >> pure (Just b)) . contramap ("cont: " <>))
-- >>> etcM () transducer' $ (Box <$> cc <*> (liftE <$> emitter'))
-- echo: hi
-- echo: bye
-- cont: echo: hi
-- cont: echo: bye

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
-- >>> let e1 = fmap (Text.pack . show) <$> (toEmit $ delayTimed (S.each (zip ((0.2*) . fromIntegral <$> [1..10]) ['a'..]))) :: Cont IO (Emitter (C.STM IO) Text)
-- >>> let e2 = fmap (Text.pack . show) <$> (toEmit $ delayTimed (S.each (zip ((0.1+) . (0.2*) . fromIntegral <$> [1..10]) (reverse ['a'..'z'])))) :: Cont IO (Emitter (C.STM IO) Text)
-- >>> let e12 = e1 <> e2
-- >>> etc () (Transducer id) $ Box <$> cStdout 6 <*> emerge ((,) <$> e1 <*> e2)
-- 'a'
-- 'z'
-- 'b'
-- 'y'
-- 'c'
-- 'x'
--
-- >>> etc () (Transducer id) $ Box <$> cStdout 6 <*> (liftA2 (<>) e1 e2)
-- 'a'
-- 'z'
-- 'b'
-- 'y'
-- 'c'
-- 'x'

-- $transduce
--
-- >>> etc () transducer' box'
-- echo: hi
-- echo: bye

-- | broadcasting
--
-- >>> (bcast, bcom) <- (C.atomically broadcast) :: IO (Broadcaster (C.STM IO) Text, Committer (C.STM IO) Text)
--
-- > (funn, fem) <- C.atomically funnel
-- >
