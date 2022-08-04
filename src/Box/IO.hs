{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | IO effects
module Box.IO
  ( fromStdin,
    toStdout,
    fromStdinN,
    toStdoutN,
    readStdin,
    showStdout,
    refCommitter,
    refEmitter,
    handleE,
    handleC,
    fileE,
    fileC,
  fileEText, fileEBS, fileCText, fileCBS)
where

import Box.Codensity
import Box.Committer
import Box.Connectors
import Box.Emitter
import qualified Control.Concurrent.Classy.IORef as C
import Control.Exception
import qualified Control.Monad.Conc.Class as C
import Data.Bool
import Data.Foldable
import Data.Functor.Contravariant
import qualified Data.Sequence as Seq
import Data.Text as Text hiding (null)
import Data.Text.IO as Text
import System.IO as IO
import Prelude
import Data.ByteString.Char8 as Char8
import Data.String

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Prelude
-- >>> import Box
-- >>> import Data.Bool
-- >>> import Data.Text (Text, pack)
-- >>> import Data.Functor.Contravariant

-- * console

-- | Emit text from stdin
--
-- @
-- λ> emit fromStdin
-- hello
-- Just "hello"
-- @
fromStdin :: Emitter IO Text
fromStdin = Emitter $ Just <$> Text.getLine

-- | Commit to stdout
--
-- >>> commit toStdout ("I'm committed!" :: Text)
-- I'm committed!
-- True
toStdout :: Committer IO Text
toStdout = Committer $ \a -> Text.putStrLn a >> pure True

-- | Finite console emitter
--
-- @
-- λ> toListM /<$|/> fromStdinN 2
-- hello
-- hello again
-- ["hello","hello again"]
-- @
fromStdinN :: Int -> CoEmitter IO Text
fromStdinN n = source n Text.getLine

-- | Finite console committer
--
-- >>> glue <$> contramap (pack . show) <$> (toStdoutN 2) <*|> qList [1..3]
-- 1
-- 2
toStdoutN :: Int -> CoCommitter IO Text
toStdoutN n = sink n Text.putStrLn

-- | Read from console, throwing away read errors
--
-- λ> glueN 2 showStdout (readStdin :: Emitter IO Int)
-- 1
-- 1
-- hippo
-- 2
readStdin :: Read a => Emitter IO a
readStdin = witherE (pure . either (const Nothing) Just) . readE $ fromStdin

-- | Show to stdout
--
-- >>> glue showStdout <$|> qList [1..3]
-- 1
-- 2
-- 3
showStdout :: Show a => Committer IO a
showStdout = contramap (Text.pack . show) toStdout

-- | Emits lines of Text from a handle.
-- handleEText = handleE Text.hGetLine

-- handleEBS = handleE Char8.hGetLine

-- | Emits lines of Text from a handle.
handleE :: (IsString a, Eq a) => (Handle -> IO a) -> Handle -> Emitter IO a
handleE action h = Emitter $ do
  l :: (Either IOException a) <- try (action h)
  pure $ case l of
    Left _ -> Nothing
    Right a -> bool (Just a) Nothing (a=="")

-- | Commit lines of Text to a handle.
handleC :: (Handle -> a -> IO ()) -> Handle -> Committer IO a
handleC action h = Committer $ \a -> do
  action h a
  pure True

-- | Commit lines of Text to a handle.
-- handleCBS = handleC Char8.hPutStrLn

-- | Emits lines of Text from a handle.
-- handleCText = handleC Text.hPutStrLn

-- | Emit lines of Text from a file.
fileE :: FilePath -> BufferMode -> IOMode -> (Handle -> Emitter IO a) -> CoEmitter IO a
fileE fp b m action = Codensity $ \eio -> withFile fp m
  (\h -> do
      hSetBuffering h b
      eio (action h))

fileEText :: FilePath -> BufferMode -> CoEmitter IO Text
fileEText fp b = fileE fp b ReadMode (handleE Text.hGetLine)

fileEBS :: FilePath -> BufferMode -> CoEmitter IO ByteString
fileEBS fp b = fileE fp b ReadMode (handleE Char8.hGetLine)

-- | Commit lines of Text to a file.
fileC :: FilePath -> IOMode -> BufferMode -> (Handle -> Committer IO a) -> CoCommitter IO a
fileC fp m b action = Codensity $ \cio -> withFile fp m
  (\h -> do
      hSetBuffering h b
      cio (action h))

fileCText :: FilePath -> BufferMode -> IOMode -> CoCommitter IO Text
fileCText fp m b = fileC fp b m (handleC Text.hPutStrLn)

fileCBS :: FilePath -> BufferMode -> IOMode -> CoCommitter IO ByteString
fileCBS fp m b = fileC fp b m (handleC Char8.hPutStrLn)

-- | Commit to an IORef
--
-- >>> (c1,l1) <- refCommitter :: IO (Committer IO Int, IO [Int])
-- >>> glue c1 <$|> qList [1..3]
-- >>> l1
-- [1,2,3]
refCommitter :: (C.MonadConc m) => m (Committer m a, m [a])
refCommitter = do
  ref <- C.newIORef Seq.empty
  let c = Committer $ \a -> do
        C.modifyIORef ref (Seq.:|> a)
        pure True
  let res = toList <$> C.readIORef ref
  pure (c, res)

-- | Emit from a list IORef
--
-- >>> e <- refEmitter [1..3]
-- >>> toListM e
-- [1,2,3]
refEmitter :: (C.MonadConc m) => [a] -> m (Emitter m a)
refEmitter xs = do
  ref <- C.newIORef xs
  let e = Emitter $ do
        as <- C.readIORef ref
        case as of
          [] -> pure Nothing
          (x : xs') -> do
            C.writeIORef ref xs'
            pure $ Just x
  pure e
