{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | IO actions
module Box.IO
  ( fromStdin,
    toStdout,
    fromStdinN,
    toStdoutN,
    readStdin,
    showStdout,
    handleE,
    handleC,
    cRef,
    eRef,
    fileE,
    fileWriteC,
    fileAppendC,
  )
where

import Box.Committer
import Box.Connectors
import Box.Cont
import Box.Emitter
import Control.Lens hiding ((.>), (:>), (<|), (|>))
import qualified Data.Sequence as Seq
import Prelude
import System.IO
import Control.Exception
import Data.Bool
import Data.Foldable
import Data.IORef

-- $setup
-- >>> :set -XOverloadedStrings

-- * console

-- | emit Text from stdin inputs
--
-- >>> :t emit fromStdin
-- emit fromStdin :: IO (Maybe String)
fromStdin :: Emitter IO String
fromStdin = Emitter $ Just <$> getLine

-- | commit to stdout
--
-- >>> commit toStdout ("I'm committed!" :: Text)
-- I'm committed!
-- True
toStdout :: Committer IO String
toStdout = Committer $ \a -> putStrLn a >> pure True

-- | finite console emitter
fromStdinN :: Int -> Cont IO (Emitter IO String)
fromStdinN n = source n getLine

-- | finite console committer
toStdoutN :: Int -> Cont IO (Committer IO String)
toStdoutN n = sink n putStrLn

-- | read from console, throwing away read errors
readStdin :: Read a => Emitter IO a
readStdin = mapE (pure . either (const Nothing) Just) . readE $ fromStdin

-- | show to stdout
showStdout :: Show a => Committer IO a
showStdout = contramap show toStdout

-- * handle operations

-- | Emits lines of Text from a handle.
handleE :: Handle -> Emitter IO String
handleE h = Emitter $ do
  (l :: Either IOException String) <- try (hGetLine h)
  pure $ case l of
    Left _ -> Nothing
    Right a -> bool (Just a) Nothing (a == "")

-- | Commit lines of Text to a handle.
handleC :: Handle -> Committer IO String
handleC h = Committer $ \a -> do
  hPutStrLn h a
  pure True

-- | Emits lines of Text from a file.
fileE :: FilePath -> Cont IO (Emitter IO String)
fileE fp = Cont $ \eio -> withFile fp ReadMode (eio . handleE)

-- | Commits lines of Text to a file.
fileWriteC :: FilePath -> Cont IO (Committer IO String)
fileWriteC fp = Cont $ \cio -> withFile fp WriteMode (cio . handleC)

-- | Commits lines of Text, appending to a file.
fileAppendC :: FilePath -> Cont IO (Committer IO String)
fileAppendC fp = Cont $ \cio -> withFile fp AppendMode (cio . handleC)

-- | commit to an IORef
cRef :: IO (Committer IO a, IO [a])
cRef = do
  ref <- newIORef Seq.empty
  let c = Committer $ \a -> do
        modifyIORef ref (Seq.:|> a)
        pure True
  let res = toList <$> readIORef ref
  pure (c, res)

-- | emit from a list IORef
eRef :: [a] -> IO (Emitter IO a)
eRef xs = do
  ref <- newIORef xs
  let e = Emitter $ do
        as <- readIORef ref
        case as of
          [] -> pure Nothing
          (x : xs') -> do
            writeIORef ref xs'
            pure $ Just x
  pure e
