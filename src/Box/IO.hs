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
    handleE,
    handleC,
    refCommitter,
    refEmitter,
    fileE,
    fileWriteC,
    fileAppendC,
  )
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
import Data.Text as Text
import Data.Text.IO as Text
import System.IO as IO
import Prelude

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
handleE :: Handle -> Emitter IO Text
handleE h = Emitter $ do
  l :: (Either IOException Text) <- try (Text.hGetLine h)
  pure $ case l of
    Left _ -> Nothing
    Right a -> bool (Just a) Nothing (a == "")

-- | Commit lines of Text to a handle.
handleC :: Handle -> Committer IO Text
handleC h = Committer $ \a -> do
  Text.hPutStrLn h a
  pure True

-- | Emit lines of Text from a file.
fileE :: FilePath -> CoEmitter IO Text
fileE fp = Codensity $ \eio -> withFile fp ReadMode (eio . handleE)

-- | Commit lines of Text to a file.
fileWriteC :: FilePath -> CoCommitter IO Text
fileWriteC fp = Codensity $ \cio -> withFile fp WriteMode (cio . handleC)

-- | Commit lines of Text, appending to a file.
fileAppendC :: FilePath -> CoCommitter IO Text
fileAppendC fp = Codensity $ \cio -> withFile fp AppendMode (cio . handleC)

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
