{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | IO actions
module Box.IO
  ( cStdin_
  , cStdin
  , cStdin'
  , eStdin
  , readStdin
  , eStdout_
  , eStdout
  , eStdout'
  , cStdout
  , showStdout
  , consolePlug
  , emitLines
  , commitLines
  , cCRef
  , cCRefM
  , toListM
  , getCommissions
  , getEmissions
  ) where

import Control.Category
import qualified Control.Foldl as L
import Control.Lens hiding ((:>), (.>), (<|), (|>))
import Data.Semigroup hiding (First, getFirst)
import qualified Data.Text.IO as Text
import Box.Box
import Box.Committer
import Box.Cont
import Box.Emitter
import Box.Plugs
import Box.Stream
import Box.Transducer
import Flow
import Protolude hiding ((.), (<>))
import Streaming (Of(..), Stream)
import qualified Streaming.Internal as S
import qualified Streaming.Prelude as S
import qualified Control.Monad.Conc.Class as C
import qualified Control.Concurrent.Classy.CRef as C

-- * console
-- | a single stdin committer action
cStdin_ :: Committer STM Text -> IO ()
cStdin_ c = do
  a <- getLine
  void $ atomically $ commit c a

-- | a finite stdin committer action
cStdin :: Int -> Committer STM Text -> IO ()
cStdin n c = replicateM_ n (cStdin_ c)

-- | a forever stdin committer action
cStdin' :: Committer STM Text -> IO ()
cStdin' = forever . cStdin_

-- | a Cont stdin emitter
eStdin :: Int -> Cont IO (Emitter STM Text)
eStdin n = cStdin n |> emitPlug

-- | read from console, throwing away read errors
readStdin :: Read a => Cont IO (Emitter STM a)
readStdin = emap (pure . either (const Nothing) Just) . eRead <$> eStdin 1000

-- | a single stdout emitter action
eStdout_ :: (Print a) => Emitter STM a -> IO ()
eStdout_ e = do
  a <- atomically $ emit e
  case a of
    Nothing -> pure ()
    Just a' -> putStrLn a'

-- | a finite stdout emitter action
eStdout :: (Print a) => Int -> Emitter STM a -> IO ()
eStdout n = replicateM_ n . eStdout_

-- | a forever stdout emitter action
eStdout' :: (Print a) => Emitter STM a -> IO ()
eStdout' = forever . eStdout_

-- | a Cont stdout committer
cStdout :: Print a => Int -> Cont IO (Committer STM a)
cStdout n = eStdout n |> commitPlug

-- | show to stdout
showStdout :: Show a => Cont IO (Committer STM a)
showStdout = contramap show <$> (cStdout 1000 :: Cont IO (Committer STM Text))

-- | console box
-- > etc () (Trans $ \s -> s & S.takeWhile (/="q") & S.map ("echo: " <>)) (console 5)
consolePlug :: Int -> Cont IO (Box STM Text Text)
consolePlug n = boxPlug (eStdout n) (cStdin n)

-- * file operations
-- | emit lines from a file
emitLines :: FilePath -> Cont IO (Emitter STM Text)
emitLines filePath = Cont (withFile filePath ReadMode) >>= fromHandle .> toEmit
  where
    fromHandle :: Handle -> Stream (Of Text) IO ()
    fromHandle h =
      forever $ do
        a <- liftIO $ Text.hGetLine h
        S.yield a

-- | commit lines to a file
commitLines :: FilePath -> Cont IO (Committer STM Text)
commitLines filePath =
  Cont (withFile filePath WriteMode) >>= toHandle .> toCommit
  where
    toHandle h = loop
      where
        loop str =
          case str of
            S.Return r -> return r
            S.Effect m -> m >>= loop
            S.Step (s :> rest) -> do
              liftIO (Text.hPutStrLn h s)
              loop rest

-- * concurrent refs
-- | commit to a list CRef
cCRef :: (C.MonadConc m) => m (C.CRef m [b], Cont m (Committer (C.STM m) b), m [b])
cCRef = do
  ref <- C.newCRef []
  let c = toCommitFold $
        L.FoldM (\x a -> C.modifyCRef x (a :) >> pure x) (pure ref) (const $ pure ())
  let res = reverse <$> C.readCRef ref
  pure (ref, c, res)

-- | commit to a monoidal CRef
cCRefM :: (C.MonadConc m, Monoid a, Semigroup a) => m (C.CRef m a, Cont m (Committer (C.STM m) a), m a)
cCRefM = do
  ref <- C.newCRef mempty
  let c = toCommitFold $
        L.FoldM (\x a -> C.modifyCRef x (a <>) >> pure x) (pure ref) (const $ pure ())
  let res = C.readCRef ref
  pure (ref, c, res)

-- | fold an emitter through a transduction, committing to a list
toListM :: (C.MonadConc m) => Cont m (Emitter (C.STM m) a) -> s -> Transducer s a b -> m ([b], s)
toListM e s t = do
  (_, c, res) <- cCRef
  r <- etc s t (Box <$> c <*> e)
  (,) <$> res <*> pure r

-- | get all commissions as a list
getCommissions :: (C.MonadConc m) => Cont m (Emitter (C.STM m) a) -> s -> Transducer s a b -> m [b]
getCommissions e s t = fst <$> toListM e s t

-- | get all emissions
getEmissions :: (C.MonadConc m) => Int -> Cont m (Emitter (C.STM m) a) -> m [a]
getEmissions n e = fst <$> toListM e () (Transducer $ S.take n)
