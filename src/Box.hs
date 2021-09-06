{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

-- | Effectful, profunctor boxes designed for concurrency.
--
-- This library follows the ideas and code from [pipes-concurrency](https://hackage.haskell.org/package/pipes-concurrency) and [mvc](https://hackage.haskell.org/package/mvc) but with some polymorphic tweaks and definitively more pretentious names.
--
-- “Boxes are surprisingly bulky. Discard or recycle the box your cell phone comes in as soon as you unpack it. You don’t need the manual or the CD that comes with it either. You’ll figure out the applications you need through using it.” — Marie Kondo
module Box
  ( -- $usage
    -- $continuations
    -- $boxes
    -- $commit
    -- $emit
    -- $state
    -- $finite
    module Box.Box,
    module Box.Committer,
    module Box.Connectors,
    module Box.Cont,
    module Box.Emitter,
    module Box.IO,
    module Box.Queue,
    module Box.Time,
  )
where

import Box.Box
import Box.Committer
import Box.Connectors
import Box.Cont
import Box.Emitter
import Box.IO
import Box.Queue
import Box.Time

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XGADTs
-- >>> :set -XFlexibleContexts
-- >>> import Data.Functor.Contravariant
-- >>> import Box
-- >>> import Control.Applicative
-- >>> import Control.Monad.Conc.Class as C
-- >>> import Control.Lens
-- >>> import qualified Data.Sequence as Seq
-- >>> import Data.Text (pack, Text)
-- >>> import Data.Functor.Contravariant
-- >>> import Data.Foldable
-- >>> import Data.Bool
-- >>> import Control.Monad.Morph
-- >>> import Control.Monad.State.Lazy

-- $usage
-- >>> :set -XOverloadedStrings
-- >>> :set -XGADTs
-- >>> :set -XFlexibleContexts
-- >>> import Data.Functor.Contravariant
-- >>> import Box
-- >>> import Control.Monad.Conc.Class as C
-- >>> import Control.Lens
-- >>> import qualified Data.Sequence as Seq
-- >>> import Data.Text (pack, Text)

-- $continuations
--
-- Continuations are very common in the API with 'Cont' as an inhouse type.
--
-- > :t fromListE [1..3::Int]
-- fromListE [1..3::Int] :: MonadConc m => Cont m (Emitter m Int)
--
-- The applicative is usually the easiest way to think about and combine continuations with their unadorned counterparts.
--
-- >>> let box' = Box <$> pure toStdout <*> fromListE (pack <$> ["a", "b"])
-- >>> :t box'
-- box' :: Cont IO (Box IO Text Text)

-- $boxes
--
-- The two basic ways of connecting up a box are related as follows:
--
-- > glue c e == glueb (Box c e)
-- > glueb == fuse (pure . pure)
--
-- >>> fromToList_ [1..3] glueb
-- [1,2,3]
--
-- >>> fromToList_ [1..3] (fuse (pure . pure))
-- [1,2,3]
--
-- 1. glue: direct fusion of committer and emitter
--
-- >>> runCont $ glue <$> pure toStdout <*> fromListE ((pack . show) <$> [1..3])
-- 1
-- 2
-- 3
--
-- Variations to the above code include:
--
-- Use of continuation applicative operators:
--
-- - the '(<*.>)' operator is short hand for runCont $ xyz 'Control.Applicative.(<*>)' zy.
--
-- - the '(<$.>)' operator is short hand for runCont $ xyz 'Control.Applicative.(<$>)' zy.
--
-- > glue <$> pure toStdout <*.> fromListE ((pack . show) <$> [1..3])
-- > glue toStdout <$.> fromListE ((pack . show) <$> [1..3])
--
-- Changing the type in the Emitter (The double fmap is cutting through the Cont and Emitter layers):
--
-- > glue toStdout <$.> fmap (fmap (pack . show)) (fromListE [1..3])
--
-- Changing the type in the committer (which is Contrvariant so needs to be a contramap):
--
-- > glue (contramap (pack . show) toStdout) <$.> fromListE [1..3]
--
-- Using the box version of glue:
--
-- > glueb <$.> (Box <$> pure toStdout <*> (fmap (pack . show) <$> fromListE [1..3]))
--
-- 2. fusion of a box, with an (a -> m (Maybe b)) function to allow for mapping, filtering and simple effects.
--
-- >>> let box' = Box <$> pure toStdout <*> fromListE ((pack . show) <$> [1..3])
-- >>> fuse (\a -> bool (pure $ Just $ "echo: " <> a) (pure Nothing) (a==("2"::Text))) <$.> box'
-- echo: 1
-- echo: 3

-- $commit
--
-- >>> commit toStdout "I'm committed!"
-- I'm committed!
-- True
--
-- Use mapC to modify a Committer and introduce effects.
--
-- >>> let c = mapC (\a -> if a==2 then (sleep 0.1 >> putStrLn "stole a 2!" >> sleep 0.1 >> pure (Nothing)) else (pure (Just a))) (contramap (pack . show) toStdout)
-- >>> glueb <$.> (Box <$> pure c <*> fromListE [1..3])
-- 1
-- stole a 2!
-- 3
--
-- The monoid instance of Committer sends each commit to both mappended committers. Because effects are also mappended together, the committed result is not always what is expected.
--
-- >>> let cFast = mapC (\b -> pure (Just b)) . contramap ("fast: " <>) $ toStdout
-- >>> let cSlow = mapC (\b -> sleep 0.1 >> pure (Just b)) . contramap ("slow: " <>) $ toStdout
-- >>> (glueb <$.> (Box <$> pure (cFast <> cSlow) <*> fromListE ((pack . show) <$> [1..3]))) <* sleep 1
-- fast: 1
-- slow: 1
-- fast: 2
-- slow: 2
-- fast: 3
-- slow: 3
--
-- To approximate what is intuitively expected, use 'concurrentC'.
--
-- >>> runCont $ (fromList_ ((pack . show) <$> [1..3]) <$> (concurrentC cFast cSlow)) <> pure (sleep 1)
-- fast: 1
-- fast: 2
-- fast: 3
-- slow: 1
-- slow: 2
-- slow: 3
--
-- This is all non-deterministic, hence the necessity for messy delays and heuristic avoidance of console races.

-- $emit
--
-- >>> ("I'm emitted!" :: Text) & Just & pure & Emitter & emit >>= print
-- Just "I'm emitted!"
--
-- >>> with (fromListE [1]) (\e' -> (emit e' & fmap show) >>= putStrLn & replicate 3 & sequence_)
-- Just 1
-- Nothing
-- Nothing
--
-- >>> toListE <$.> (fromListE [1..3])
-- [1,2,3]
--
-- The monoid instance is left-biased.
--
-- >>> toListE <$.> (fromListE [1..3] <> fromListE [7..9])
-- [1,2,3,7,8,9]
--
-- Use concurrentE to get some nondeterministic balance.
--
-- > let es = (join $ concurrentE <$> (fromListE [1..3]) <*> (fromListE [7..9]))
-- > glue (contramap (pack . show) toStdout) <$.> es
-- 1
-- 2
-- 7
-- 3
-- 8
-- 9

-- $state
--
-- State committers and emitters are related as follows:
--
-- >>> toList . fst $ runIdentity $ flip execStateT (Seq.empty,Seq.fromList [1..4]) $ glue (hoist (zoom _1) stateC) (hoist (zoom _2) stateE)
-- [1,2,3,4]
--
-- For some reason, related to a lack of an MFunctor instance for Cont, but exactly not yet categorically pinned to a wall, the following compiles but is wrong.
--
-- >>> flip runStateT (Seq.empty) $ runCont $ glue <$> pure stateC <*> fromListE [1..4]
-- ((),fromList [])

-- $finite
--
-- Most committers and emitters will run forever until:
--
-- - the glued or fused other-side returns.
-- - the Transducer, stream or monadic action returns.
--
-- Finite ends (collective noun for emitters and committers) can be created with 'sink' and 'source' eg
--
-- >>> glue <$> contramap show <$> (sink 5 putStrLn) <*.> fromListE [1..]
-- 1
-- 2
-- 3
-- 4
-- 5
--
-- Two infinite ends will tend to run infinitely.
--
-- > glue <$> pure (contramap (pack . show) toStdout) <*.> fromListE [1..]
--
-- 1
-- 2
-- ...
-- 💁
-- ∞
--
-- Use glueN to create a finite computation.
--
-- >>> glueN 4 <$> pure (contramap (pack . show) toStdout) <*.> fromListE [1..]
-- 1
-- 2
-- 3
-- 4
