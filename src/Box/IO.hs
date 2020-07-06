{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | IO actions
module Box.IO
  ( fromStdin,
    toStdout,
    cStdin_,
    cStdin,
    cStdin',
    eStdin,
    readStdin,
    eStdout_,
    eStdout,
    eStdoutM,
    eStdout',
    cStdout,
    cStdout',
    eStdin',
    showStdout,
    Box.IO.getLine,
    putLine,
    consolePlug,
    emitLines,
    commitLines,
    cCRef,
    cCRefM,
    toListM,
    getCommissions,
    getEmissions,
    fileEmitter,
    fileCommitter,
    appendCommitter,
  )
where

import Box.Box
import Box.Committer
import Box.Cont
import Box.Emitter
import Box.Plugs
import Box.Stream
import Box.Transducer
import qualified Control.Concurrent.Classy.IORef as C
import qualified Control.Foldl as L
import Control.Lens hiding ((.>), (:>), (<|), (|>))
import qualified Control.Monad.Conc.Class as C
import Control.Monad.Conc.Class (STM)
import Data.Text.IO (hGetLine)
import NumHask.Prelude hiding (STM)
import qualified Streaming.Prelude as S

-- $setup
-- >>> :set -XOverloadedStrings

-- * console
-- | emit Text from stdin inputs
--
-- >>> :t emit fromStdin
-- emit fromStdin :: IO (Maybe Text)
--
fromStdin :: Emitter IO Text
fromStdin = Emitter $ Just <$> NumHask.Prelude.getLine

-- | commit to stdout
--
-- >>> commit toStdout ("I'm committed!" :: Text)
-- I'm committed!
-- True
--
toStdout :: Committer IO Text
toStdout = Committer $ \a -> putStrLn a *> pure True

-- | a single stdin committer action
cStdin_ :: Committer (STM IO) Text -> IO ()
cStdin_ c = do
  a <- NumHask.Prelude.getLine
  void $ C.atomically $ commit c a

-- | a finite stdin committer action
cStdin :: Int -> Committer (STM IO) Text -> IO ()
cStdin n c = replicateM_ n (cStdin_ c)

-- | a forever stdin committer action
cStdin' :: Committer (STM IO) Text -> IO ()
cStdin' = forever . cStdin_

-- | a Cont stdin emitter
eStdin :: Int -> Cont IO (Emitter (STM IO) Text)
eStdin n = cStdin n & emitPlug

-- | read from console, throwing away read errors
readStdin :: Read a => Cont IO (Emitter (STM IO) a)
readStdin = emap (pure . either (const Nothing) Just) . eRead <$> eStdin 1000

-- | a single stdout emitter action
eStdout_ :: Emitter (STM IO) Text -> IO ()
eStdout_ e = do
  a <- C.atomically $ emit e
  case a of
    Nothing -> pure ()
    Just a' -> putStrLn a'

-- | a single stdout emitter action
eStdoutM_ :: Emitter IO Text -> IO ()
eStdoutM_ e = do
  a <- emit e
  case a of
    Nothing -> pure ()
    Just a' -> putStrLn a'

-- | a finite stdout emitter action
eStdout :: Int -> Emitter (STM IO) Text -> IO ()
eStdout n = replicateM_ n . eStdout_

-- | a finite stdout emitter action
eStdoutM :: Int -> Emitter IO Text -> IO ()
eStdoutM n = replicateM_ n . eStdoutM_

-- | a forever stdout emitter action
eStdout' :: Emitter (STM IO) Text -> IO ()
eStdout' = forever . eStdout_

-- | a Cont stdout committer
cStdout :: Int -> Cont IO (Committer (STM IO) Text)
cStdout n = eStdout n & commitPlug

-- | a Cont stdout committer
cStdout' :: Cont IO (Committer (STM IO) Text)
cStdout' = eStdout' & commitPlug

-- | a Cont stdin emitter
eStdin' :: Cont IO (Emitter (STM IO) Text)
eStdin' = cStdin' & emitPlug

-- | show to stdout
showStdout :: Show a => Cont IO (Committer (STM IO) a)
showStdout = contramap (pack . show) <$> (cStdout 1000 :: Cont IO (Committer (STM IO) Text))

-- | Get a single line from a handle.
getLine_ :: Handle -> Committer (STM IO) Text -> IO ()
getLine_ h c = do
  a <- hGetLine h
  void $ C.atomically $ commit c a

-- | Get lines from a handle forever.
getLine :: Handle -> Committer (STM IO) Text -> IO ()
getLine h = forever . getLine_ h

-- | Put a single line to a handle.
putLine_ :: Handle -> Emitter (STM IO) Text -> IO ()
putLine_ h e = do
  a <- C.atomically $ emit e
  case a of
    Nothing -> pure ()
    Just a' -> hPutStrLn h a'

-- | a forever getLine committer action
putLine :: Handle -> Emitter (STM IO) Text -> IO ()
putLine h = forever . putLine_ h

-- | console box
-- > etc () (Trans $ \s -> s & S.takeWhile (/="q") & S.map ("echo: " <>)) (console 5)
consolePlug :: Int -> Cont IO (Box (STM IO) Text Text)
consolePlug n = boxPlug (eStdout n) (cStdin n)

-- * file operations

-- | Emits lines of Text from a handle.
emitLines :: Handle -> Emitter IO Text
emitLines h = Emitter $ do
  l :: (Either IOException Text) <- try (hGetLine h)
  pure $ case l of
    Left _ -> Nothing
    Right a -> bool (Just a) Nothing (a == "")

-- | Commit lines of Text to a handle.
commitLines :: Handle -> Committer IO Text
commitLines h = Committer $ \a -> do
  hPutStrLn h a
  pure True

fileEmitter :: FilePath -> Cont IO (Emitter IO Text)
fileEmitter fp = Cont $ \eio -> withFile fp ReadMode (eio . emitLines)

fileCommitter :: FilePath -> Cont IO (Committer IO Text)
fileCommitter fp = Cont $ \cio -> withFile fp WriteMode (cio . commitLines)

appendCommitter :: FilePath -> Cont IO (Committer IO Text)
appendCommitter fp = Cont $ \cio -> withFile fp AppendMode (cio . commitLines)

-- * concurrent refs

-- | commit to a list CRef
cCRef :: (C.MonadConc m) => m (C.IORef m [b], Cont m (Committer (C.STM m) b), m [b])
cCRef = do
  ref <- C.newIORef []
  let c =
        toCommitFold $
          L.FoldM (\x a -> C.modifyIORef x (a :) >> pure x) (pure ref) (const $ pure ())
  let res = reverse <$> C.readIORef ref
  pure (ref, c, res)

-- | commit to a monoidal CRef
cCRefM :: (C.MonadConc m, Monoid a) => m (C.IORef m a, Cont m (Committer (C.STM m) a), m a)
cCRefM = do
  ref <- C.newIORef mempty
  let c =
        toCommitFold $
          L.FoldM (\x a -> C.modifyIORef x (a <>) >> pure x) (pure ref) (const $ pure ())
  let res = C.readIORef ref
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
