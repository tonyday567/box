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
import qualified Control.Concurrent.Classy.IORef as C
import Control.Lens hiding ((.>), (:>), (<|), (|>))
import qualified Control.Monad.Conc.Class as C
import Data.Text.IO (hGetLine)
import NumHask.Prelude hiding (STM)

-- $setup
-- >>> :set -XOverloadedStrings

-- * console

-- | emit Text from stdin inputs
--
-- >>> :t emit fromStdin
-- emit fromStdin :: IO (Maybe Text)
fromStdin :: Emitter IO Text
fromStdin = Emitter $ Just <$> NumHask.Prelude.getLine

-- | commit to stdout
--
-- >>> commit toStdout ("I'm committed!" :: Text)
-- I'm committed!
-- True
toStdout :: Committer IO Text
toStdout = Committer $ \a -> putStrLn a >> pure True

-- | finite console emitter
fromStdinN :: Int -> Cont IO (Emitter IO Text)
fromStdinN n = source n NumHask.Prelude.getLine

-- | finite console committer
toStdoutN :: Int -> Cont IO (Committer IO Text)
toStdoutN n = sink n putStrLn

-- | read from console, throwing away read errors
readStdin :: Read a => Emitter IO a
readStdin = mapE (pure . either (const Nothing) Just) . readE $ fromStdin

-- | show to stdout
showStdout :: Show a => Committer IO a
showStdout = contramap show toStdout

-- * handle operations
-- | Emits lines of Text from a handle.
handleE :: Handle -> Emitter IO Text
handleE h = Emitter $ do
  l :: (Either IOException Text) <- try (hGetLine h)
  pure $ case l of
    Left _ -> Nothing
    Right a -> bool (Just a) Nothing (a == "")

-- | Commit lines of Text to a handle.
handleC :: Handle -> Committer IO Text
handleC h = Committer $ \a -> do
  hPutStrLn h a
  pure True

-- | Emits lines of Text from a file.
fileE :: FilePath -> Cont IO (Emitter IO Text)
fileE fp = Cont $ \eio -> withFile fp ReadMode (eio . handleE)

-- | Commits lines of Text to a file.
fileWriteC :: FilePath -> Cont IO (Committer IO Text)
fileWriteC fp = Cont $ \cio -> withFile fp WriteMode (cio . handleC)

-- | Commits lines of Text, appending to a file.
fileAppendC :: FilePath -> Cont IO (Committer IO Text)
fileAppendC fp = Cont $ \cio -> withFile fp AppendMode (cio . handleC)

-- | commit to a list IORef
cRef :: (C.MonadConc m) => m (Committer m a, m [a])
cRef = do
  ref <- C.newIORef []
  let c = Committer $ \a -> do
        C.modifyIORef ref (a :)
        pure True
  let res = reverse <$> C.readIORef ref
  pure (c, res)

-- | emit from a list IORef
eRef :: (C.MonadConc m) => [a] -> m (Emitter m a)
eRef xs = do
  ref <- C.newIORef xs
  let e = Emitter $ do
        as <- C.readIORef ref
        case as of
          [] -> pure Nothing
          (x : xs') -> do
            C.writeIORef ref xs'
            pure $ Just x
  pure e
