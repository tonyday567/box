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
module Etc.IO
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
  , cIORef
  , cIORefM
  , toListIO
  , getCommissions
  , getEmissions
  ) where

import Control.Category
import qualified Control.Foldl as L
import Control.Lens hiding ((:>), (.>), (<|), (|>))
import Data.IORef
import Data.Semigroup hiding (First, getFirst)
import qualified Data.Text.IO as Text
import Etc.Box
import Etc.Committer
import Etc.Cont
import Etc.Emitter
import Etc.Stream
import Etc.Transducer
import Flow
import Protolude hiding ((.), (<>))
import Streaming (Of(..), Stream)
import qualified Streaming.Internal as S
import qualified Streaming.Prelude as S

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

-- | read from console
readStdin :: Read a => Cont IO (Emitter STM a)
readStdin = maybeEmit (pure . either (const Nothing) Just) <$> eRead (eStdin 1000)

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

-- * iorefs
-- | commit to a list IORef
cIORef :: IORef [b] -> Cont IO (Committer STM b)
cIORef ref =
  toCommitFold $
  L.FoldM (\x a -> modifyIORef x (a :) >> pure x) (pure ref) (const $ pure ())

-- | commit to a monoidal IORef
cIORefM :: Semigroup a => IORef a -> Cont IO (Committer STM a)
cIORefM ref =
  toCommitFold $
  L.FoldM (\x a -> modifyIORef x (a <>) >> pure x) (pure ref) (const $ pure ())

-- | fold an emitter through a transduction, committing to a list
toListIO :: Cont IO (Emitter STM a) -> s -> Transducer s a b -> IO ([b], s)
toListIO e s t = do
  ref <- newIORef []
  r <- etc s t (Box <$> cIORef ref <*> e)
  (,) <$> readIORef ref <*> pure r

-- | get all commissions as a list
getCommissions :: Cont IO (Emitter STM a) -> s -> Transducer s a b -> IO [b]
getCommissions e s t = fst <$> toListIO e s t

-- | get all emissions
getEmissions :: Int -> Cont IO (Emitter STM a) -> IO [a]
getEmissions n e = fst <$> toListIO e () (Transducer $ S.take n)
