{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | `emit` - `transduce` - `commit`
--
-- This library is a combination of ideas and code from [pipes-concurrency](https://hackage.haskell.org/package/pipes-concurrency) and [mvc](https://hackage.haskell.org/package/mvc) but with pretentious names.
--
-- > import Etc
-- > import qualified Streaming.Prelude as S
-- > let committer' = cStdout 100 unbounded
-- > let emitter' = toEmit (bounded 1) (S.each ["hi","bye","q","x"])
-- > let box' = Box <$> committer' <*> emitter'
-- > let transducer' = Transducer $ \s -> s & S.takeWhile (/="q") & S.map ("echo: " <>)
-- > etc () transducer' box'
-- echo: hi
-- echo: bye
--
-- https://www.schoolofhaskell.com/school/advanced-haskell/beautiful-concurrency/3-software-transactional-memory
-- 
module Etc
  ( module Etc.Box
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


