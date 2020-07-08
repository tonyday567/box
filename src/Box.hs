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
module Box
  ( -- $setup
    -- $emit
    -- $commit
    -- $state
    -- $splitting
    module Box.Box,
    module Box.Broadcast,
    module Box.Committer,
    module Box.Connectors,
    module Box.Cont,
    module Box.Emitter,
    module Box.IO,
    module Box.Queue,
    module Box.Time,
    module Box.Transducer,
    module Box.Updater,
  )
where

import Box.Box
import Box.Broadcast
import Box.Committer
import Box.Connectors
import Box.Cont
import Box.Emitter
import Box.IO
import Box.Queue
import Box.Time
import Box.Transducer
import Box.Updater

{- $setup
>>> :set -XOverloadedStrings
>>> :set -XGADTs
>>> :set -XNoImplicitPrelude
>>> :set -XFlexibleContexts
>>> import NumHask.Prelude
>>> import qualified Prelude as P
>>> import Data.Functor.Contravariant
>>> import Box
>>> import qualified Streaming.Prelude as S
>>> import Control.Monad.Conc.Class as C
>>> import Control.Lens
>>> let emitter' = fromListE ["hi","bye","q","x" :: Text]

Continuations are very common in the API.
>>> :t emitter'
emitter' :: MonadConc m => Cont m (Emitter m Text)

>>> let box' = Box <$> pure toStdout <*> emitter'
>>> :t box'
box' :: Cont IO (Box IO Text Text)

>>> let transducer' = Transducer $ \s -> s & S.takeWhile (/="q") & S.map ("echo: " <>)

The basic ways of connecting up emitters to committers obey the following law:

> glue c e == fuse (pure . pure) (Box c e) == etc () (Transducer id) (Box c e)

1. glue: direct fusion of committer and emitter

>>> glue <$> pure toStdout <*.> emitter'
hi
bye
q
x

2. fusion of a box, with an (a -> m (Maybe b)) function to allow for mapping, filtering and simple effects.

>>> fuse (\a -> bool (pure $ Just $ "echo: " <> a) (pure Nothing) (a==("q"::Text))) <$.> box'
echo: hi
echo: bye
echo: x

3. using the streaming library and the Transducer abstraction.

>>> let transducer' = Transducer $ \s -> s & S.takeWhile (/="q") & S.map ("echo: " <>)
>>> etc () transducer' <$.> box'
echo: hi
echo: bye

-}

{- | $emit

>>> ("I'm emitted!" :: Text) & Just & pure & Emitter & emit >>= print
Just "I'm emitted!"

>>> with (fromListE [1]) (\e' -> (emit e' & fmap show :: IO Text) >>= putStrLn & replicate 3 & sequence_)
Just 1
Nothing
Nothing

> toListE <$.> fromListE xs == pure xs

>>> toListE <$.> (fromListE [1..3])
[1,2,3]

The monoid instance is left-biased.

>>> toListE <$.> (fromListE [1..3] <> fromListE [7..9])
[1,2,3,7,8,9]

-}

{- | $commit

>>> commit toStdout "something"
something
True

The monoid instance of Committer sends each commit to both mappended committers. Because everything is concurrent, race effects are common on stdout, so we introduce some delaying effects to (hopefully) avoid races.

>>> let cFast = cmap (\b -> sleep 0.01 >> pure (Just b)) . contramap ("fast: " <>) $ toStdout
>>> let cSlow = cmap (\b -> sleep 0.1 >> pure (Just b)) . contramap ("slow: " <>) $ toStdout
>>> (etc () transducer' <$.> (Box <$> pure (cFast <> cSlow) <*> emitter')) >> sleep 1
fast: echo: hi
slow: echo: hi
fast: echo: bye
slow: echo: bye

>>> let e = fromListE ["hi","bye","q","x"]
>>> let c = cmap (\a -> if a=="q" then (sleep 0.1 >> putStrLn "stolen!" >> sleep 0.1 >> pure (Nothing)) else (pure (Just a))) toStdout
>>> fuse (pure . pure) <$.> (Box <$> pure c <*> e)
hi
bye
stolen!
x

-}

{- $state

State committers and emitters are related as follows:

>>> runIdentity $ fmap (reverse . fst) $ flip execStateT ([],[1..4]) $ glue (hoist (zoom _1) stateC) (hoist (zoom _2) stateE)
[1,2,3,4]

-}


{- | $splitting

prism handler

>>> import Control.Lens (_Right)
>>> let e = fromListE ["hi","bye","q","x"]
>>> let e2 = (fmap (\x -> Right (x <> "_right")) <$> e) <> (fmap (\x -> Left (x <> "_left")) <$> e)
>>> let cright = handles _Right toStdout
>>> fuse (pure . pure) <$.> (Box <$> pure cright <*> e2)
hi_right
bye_right
q_right
x_right

splitCommit
Splits a committer into two.

>>> let cs = splitCommit $ pure toStdout
>>> let cc = contCommit <$> cs <*> pure (cmap (\b -> sleep 0.1 >> pure (Just b)) . contramap ("cont: " <>))

> (etc () transducer' $ (Box <$> cc <*> emitter')) >> sleep 0.5
echo: hi
echo: bye
cont: echo: hi
cont: echo: bye

>>> let c = toStdoutN 10
>>> let e = fromListE ["hi","bye","q","x"]
>>> let e' = emap (\a -> if a=="q" then (sleep 0.1 >> putStrLn "stole a q!" >> sleep 0.1 >> pure (Nothing)) else (pure (Just a))) <$> e :: Cont IO (Emitter IO Text)
>>> fuse (pure . pure) <$.> (Box <$> c <*> e')
hi
bye
stole a q!
x

emerge

> let e1 = fmap (pack . show) <$> (toEmitter $ delayTimed (S.each (zip ((0.2 P.*) . P.fromIntegral <$> [1..10]) ['a'..]))) :: Cont IO (Emitter IO Text)
> let e2 = fmap (pack . show) <$> (toEmitter $ delayTimed (S.each (zip ((0.1 P.+) . (0.2 P.*) . P.fromIntegral <$> [1..10]) (reverse ['a'..'z'])))) :: Cont IO (Emitter IO Text)
> let e12 = e1 <> e2
> etc () (Transducer (S.take 6)) <$.> Box <$> toStdout <*> emerge ((,) <$> e1 <*> e2)
'a'
'z'
'b'
'y'
'c'
'x'

> etc () (Transducer id) <$.> Box <$> pure toStdout <*> (liftA2 (<>) e1 e2)
'a'
'z'
'b'
'y'
'c'
'x'

-}


