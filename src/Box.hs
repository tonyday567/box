{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

-- | An effect system, designed for concurrency.
--
-- “Boxes are surprisingly bulky. Discard or recycle the box your cell phone comes in as soon as you unpack it. You don’t need the manual or the CD that comes with it either. You’ll figure out the applications you need through using it.” — Marie Kondo
module Box
  ( -- $usage
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
-- >>> import Box
-- >>> import Prelude
-- >>> pushList <$|> qList [1,2,3]
-- [1,2,3]
--
-- >>> glue toStdout <$|> qList ["a", "b", "c"]
-- a
-- b
-- c

-- $usage
-- >>> :set -XOverloadedStrings
-- >>> import Box
-- >>> import Prelude
-- >>> pushList <$|> qList [1,2,3]
-- [1,2,3]
--
-- >>> glue toStdout <$|> qList ["a", "b", "c"]
-- a
-- b
-- c
