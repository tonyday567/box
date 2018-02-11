{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- emit transduce commit

module Etc where

import Control.Category
import Control.Monad.Managed
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.IORef
import Data.Profunctor
import Flow
import Pipes.Concurrent as P
import Protolude hiding ((.))
import Streaming (Of(..), Stream)
import qualified Control.Foldl as L
import qualified Data.Text.IO as Text
import qualified Streaming as S
import qualified Streaming.Internal as S
import qualified Streaming.Prelude as S

-- | emit
newtype Emitter a = Emitter (Input a) deriving (Functor, Applicative, Monad, Alternative, MonadPlus, Monoid)

-- | maybe emit a value from an emitter
emit' :: Emitter a -> IO (Maybe a)
emit' (Emitter i) = atomically $ recv i

-- | emit from an emitter
emit :: Emitter a -> IO a
emit (Emitter i) = do
  a <- atomically $ recv i
  case a of
    Nothing -> emit (Emitter i)
    Just a' -> pure a'

-- | commit
newtype Committer a = Committer (Output a) deriving (Monoid, Contravariant, Divisible, Decidable)

-- | commit an a to a committer
commit :: Committer a -> a -> IO ()
commit (Committer o) = void . atomically . send o

-- | combining a Committer and Emitter gets you close to a black box with two wires hanging off of it for input and output, but where you can't look inside the box.
-- the box metaphor is completely reversable: it can be used in situations where the object of attention should be thought about as "inside the box" communicating with the outside world via two thin wires, or as "outside of a box" trying to use an opaque process.
data Box c e = Box (Committer c) (Emitter e)

instance Profunctor Box where
  dimap f g (Box c e) = Box (contramap f c) (fmap g e) 

instance Monoid (Box c e) where
    mempty = Box mempty mempty
    mappend (Box c e)  (Box c' e') = Box (c <> c') (e <> e')

{-
instance Category Box where
  (Box c e) . (Box c' e') = unsafePerformIO (fuse e' c >> pure (Box c' e))
  id = undefined
-}

fuse :: Emitter a -> Committer a -> IO ()
fuse (Emitter e) (Committer c) = forever $ atomically $ do
    a <- recv e
    maybe (pure ()) (void <$> send c) a

-- | transduction
newtype Trans s a b = Trans (forall m . Monad m =>
  Stream (Of a) (StateT s m) () ->
  Stream (Of b) (StateT s m) ())

instance Category (Trans s) where
    (Trans t1) . (Trans t2) = Trans (t1 . t2)
    id = Trans id

-- $setup
-- >>> let t = (Trans $ \s -> s & S.takeWhile (/="q") & S.map ("echo: " <>))
-- >>> let b = (Box <$> cStdout 5 <*> toEmit (S.each ["hi","bye","q","x"]))
--

-- | emit - transduce - commit
-- >>> etc () t b
-- echo: hi
-- echo: bye
--
etc ::
  s ->
  Trans s a b ->
  Managed (Box b a) ->
  IO s
etc st (Trans stream) ea =
  with ea $ \(Box committer emitter) ->
    ( emitter |>
      toStream |>
      stream |>
      fromStream)
    committer |>
    flip execStateT st

-- | turn an emitter into a stream
toStream :: (S.MonadBase IO m) => Emitter a -> Stream (Of a) m ()
toStream (Emitter e) = S.untilRight getNext
  where
    getNext = maybe (Right ()) Left <$> S.liftBase (atomically $ recv e)

-- | turn a stream into a committer
fromStream :: (S.MonadBase IO m) => Stream (Of b) m () -> Committer b -> m ()
fromStream s (Committer write) = go s
  where
    go str = do
      eNxt <- S.next str -- uncons requires r ~ ()
      forM_ eNxt $ \(a, str') -> do
        continue <- S.liftBase (atomically (send write a))
        when continue (go str')

-- | create an emitter from a stream
toEmit :: Stream (Of a) IO () -> Managed (Emitter a)
toEmit s = Emitter <$> managed
  (P.withBuffer P.unbounded (Committer .> fromStream s) .> fmap snd)

-- | create a committer from a stream consumer
toCommit :: (Stream (Of a) IO () -> IO r) -> Managed (Committer a)
toCommit f = Committer <$> managed
  (\c -> P.withBuffer P.unbounded c (Emitter .> toStream .> f) |> fmap fst)

-- | create a committer from a fold
toCommitFold :: L.FoldM IO a () -> Managed (Committer a)
toCommitFold f = toCommit (L.impurely S.foldM f .> fmap S.snd')

-- | insert a buffer into a stream
-- todo: generalize
buffStream :: Stream (Of a) IO () -> (Stream (Of a) IO () -> IO r) -> IO r
buffStream i o =
  fmap snd $ P.withBuffer P.unbounded
  (Committer .> fromStream i)
  (Emitter .> toStream .> o)

-- | fuse a committer io and emitter io with a buffer
buff :: (Committer a -> IO b) -> (Emitter a -> IO c) -> IO c
buff c e =
  fmap snd $ P.withBuffer P.unbounded (c <. Committer) (e <. Emitter)

waitCancel :: IO b -> IO a -> IO b
waitCancel a b =
  withAsync a $ \a' ->
  withAsync b $ \b' -> do
  a'' <- wait a'
  cancel b'
  pure a''

buff' :: Buffer a -> (Input a -> IO i) -> (Output a -> IO r) -> IO r
buff' b c e = withSpawn b
  (\(i,o) ->
     waitCancel
     (e i)
     (c o)
  )

-- | create an stdin committer
cStdin :: Int -> Committer Text -> IO ()
cStdin n (Committer c) = replicateM_ n $ do
  a' <- getLine
  atomically $ send c a'

-- | hook a committer through a basket, creating a managed emitter
wireEmit :: (Committer a -> IO r) -> Managed (Emitter a)
wireEmit c = managed $ \e ->
  P.withBuffer P.unbounded (c . Committer) (e . Emitter) |> fmap snd

-- | a managed stdin emitter
eStdin :: Int -> Managed (Emitter Text)
eStdin n = cStdin n |> wireEmit

-- | create an stdout emitter
eStdout :: Print a => Int -> Emitter a -> IO ()
eStdout n (Emitter e) =
  (e |> recv |> atomically) >>= maybe (pure ()) putStrLn |> replicateM_ n

-- | hook an emitter through a basket, creating a managed committer
wireCommit :: (Emitter a -> IO ()) -> Managed (Committer a)
wireCommit e = managed $ \c ->
  P.withBuffer P.unbounded (c . Committer) (e . Emitter) |> fmap fst

-- | a managed stdout committer
cStdout :: Print a => Int -> Managed (Committer a)
cStdout n = eStdout n |> wireCommit

toBox :: (Emitter a -> IO ()) -> (Committer b -> IO ()) -> Managed (Box a b)
toBox eio cio = Box <$> wireCommit eio <*> wireEmit cio

-- | console box
-- > etc () (Trans $ \s -> s & S.takeWhile (/="q") & S.map ("echo: " <>)) (console 5)
console :: Int -> Managed (Box Text Text)
console x = toBox (eStdout x) (cStdin x)

-- | emit lines from a file
emitLines :: FilePath -> Managed (Emitter Text)
emitLines filePath =
  managed (withFile filePath ReadMode) >>= fromHandle .> toEmit
  where
    fromHandle :: Handle -> Stream (Of Text) IO ()
    fromHandle h = forever $ do
      a <- liftIO $ Text.hGetLine h
      S.yield a

-- | commit lines to a file
commitLines :: FilePath -> Managed (Committer Text)
commitLines filePath =
  managed (withFile filePath WriteMode) >>= toHandle .> toCommit
  where
    toHandle h = loop where
      loop str = case str of
        S.Return r         -> return r
        S.Effect m          -> m >>= loop
        S.Step (s :> rest) -> do
          liftIO (Text.hPutStrLn h s)
          loop rest

-- | commit to a monoidal IORef
ioRef :: Monoid m => IORef m -> Managed (Committer m)
ioRef ref = toCommitFold $ L.FoldM (\x a -> modifyIORef x (a <>) >> pure x) (pure ref) (const $ pure ())

-- | fold an emitter through a model, to a list and state
foldToList :: Managed (Emitter a) -> s -> Trans s a b -> IO ([b], s)
foldToList e s t = do
    ref <- newIORef []
    r <- etc s t (Box <$> contramap (:[]) <$> ioRef ref <*> e)
    (,) <$> readIORef ref <*> pure r

-- | fold an emitter thru a model, to a list, ignoring the end state
foldToList_ :: Managed (Emitter a) -> s -> Trans s a b -> IO [b]
foldToList_ e s t = fmap fst $ foldToList e s t

-- | sleep for x seconds
sleep :: Double -> IO ()
sleep x = threadDelay (floor $ x * 1e6)

-- | sleep for x seconds, then send a q
quitter :: Double -> a -> Managed (Emitter a)
quitter x q = do
  liftIO $ sleep x
  S.yield q |> toEmit

-- | split a managed box into commision and emission threads
-- todo: this may not be necessary, and doesn't work
-- > etc () t (asyncer b)
--
asyncer :: (Show a, Show b) =>
  Managed (Box a b) ->
  Managed (Box a b)
asyncer r = managed $ \rio ->
  with r $
    \(Box c e) -> do
      ac <- async (do
                   putStrLn ("ac" :: Text)
                   res <- rio (Box c mempty)
                   putStrLn ("zc" :: Text)
                   pure res)
      sleep 2
      ae <- async (do
                   putStrLn ("ae" :: Text)
                   res <- rio (Box mempty e)
                   putStrLn ("ze" :: Text)
                   pure res)
      (_, ac') <- waitBoth ac ae
      putStrLn ("$" :: Text)
      pure ac'

-- | async a managed box
-- >>> etc () t (asyncer' b)
-- ac
-- zc
-- echo: hi
-- echo: bye
-- *
--
asyncer' :: (Show a, Show b) =>
  Managed (Box a b) ->
  Managed (Box a b)
asyncer' r = managed $ \rio ->
  with r $
    \(Box c e) -> do
      ac <- async (do
                   putStrLn ("ac" :: Text)
                   res <- rio (Box c e)
                   putStrLn ("zc" :: Text)
                   pure res)
      sleep 0.1
      ac' <- wait ac
      putStrLn ("*" :: Text)
      pure ac'

-- runWrapMVC, runAction, ActionComms, runWrapWith', vcs, timeOut, stdinParsed
-- MVC.Action
