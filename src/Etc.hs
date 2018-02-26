{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
module Etc
  ( Emitter(..)
  , Committer(..)
  , Box(..)
  , BoxM(..)
  , fuse_
  , fuse
  , Transducer(..)
  , etc
  , toStream
  , fromStream
  , withBuffer
  , withBufferE
  , withBufferC
  , Buffer(..)
  , bounded
  , unbounded
  , latest
  , newest
  , buffCommit
  , buffEmit
  , toBox
  , toBoxForget
  , toCommit
  , toCommitFold
  , toCommitIO
  , toEmit
  , buffC
  , buffE
  , fuseEmit
  , fuseCommit
  , branchE
  , alternate
  , cStdin_
  , cStdin
  , cStdin'
  , eStdin
  , readStdin
  , eStdout_
  , eStdout
  , eStdout'
  , cStdout
  , showStdout
  , console
  , emitLines
  , commitLines
  , cIORef
  , cIORefM
  , toListIO
  , getCommissions
  , getEmissions
  , feedback
  , keeps
  , handles
  , eParse
  , eRead
  , eRead'
  , cShow
  , asPipe
  ) where

import Control.Category
import Control.Lens hiding ((:>), (.>), (|>), (<|))
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Managed
import Data.Functor.Constant
import Data.Functor.Contravariant.Divisible
import Data.IORef
import Data.Semigroup hiding (First, getFirst)
import Flow
import Protolude hiding ((.), (<>))
import Streaming (Of(..), Stream)
import Text.Read (reads)
import qualified Control.Concurrent.Async
import qualified Control.Concurrent.STM as S
import qualified Control.Exception
import qualified Control.Foldl as L
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Pipes
import qualified Pipes.Prelude as Pipes
import qualified Streaming.Internal as S
import qualified Streaming.Prelude as S
-- import qualified Control.Concurrent.STM as STM

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import Etc
-- >>> import qualified Streaming.Prelude as S
-- >>> let committer' = cStdout 100 unbounded
-- >>> let emitter' = toEmit (bounded 1) (S.each ["hi","bye","q","x"])
-- >>> let box' = Box <$> committer' <*> emitter'
-- >>> let transducer' = Transducer $ \s -> s & S.takeWhile (/="q") & S.map ("echo: " <>)
--

-- | an Emitter emits values of type a. A Source of 'a's & a Producer of 'a's are the two obvious alternative but overloaded metaphors out there. It is a newtype warpper around pipes-concurrency Input to pick up the instances.
--
-- Naming is reversed in comparison to the 'Input' it wraps.  An Emitter 'reaches into itself' for the value to emit, where itself is an opaque thing from the pov of usage.  An Emitter is named for its' main action: it emits.
--
-- >>> with (S.each [0..] & toEmit unbounded) emit >>= print
-- Just 0
--
newtype Emitter a = Emitter
  { emit :: IO (Maybe a)
  }

instance Functor Emitter where
  fmap f m = Emitter (fmap (fmap f) (emit m))

instance Applicative Emitter where
  pure r = Emitter (pure (pure r))
  mf <*> mx = Emitter ((<*>) <$> emit mf <*> emit mx)

instance Monad Emitter where
  return r = Emitter (return (return r))
  m >>= f =
    Emitter $ do
      ma <- emit m
      case ma of
        Nothing -> return Nothing
        Just a -> emit (f a)

instance Alternative Emitter where
  empty = Emitter (return Nothing)
  x <|> y =
    Emitter $ do
      (i, ma) <- fmap ((,) y) (emit x) <|> fmap ((,) x) (emit y)
      case ma of
        Nothing -> emit i
        Just a -> return (Just a)

instance MonadPlus Emitter where
  mzero = empty
  mplus = (<|>)

instance Semigroup (Emitter a) where
  (<>) = (<|>)

instance Monoid (Emitter a) where
  mempty = empty
  mappend = (<>)

-- | a Committer commits values of type a (to the effects void presumably). A Sink for 'a's & a Consumer of 'a's are the other metaphors. It is a newtype wrapper around pipes-concurrency Output to pick up the instances.
--
-- Naming is reversed in comparison to the 'Output' it wraps.  An Committer 'reaches out and absorbs' the value being committed; the value disappears into the opaque thing that is a Committer from the pov of usage.  An Committer is named for its' main action: it commits.
--
-- >>> import Etc.Time
-- >>> with (cStdout 100 unbounded) $ \c -> commit c "something" >> sleep 1
-- something
--
newtype Committer a = Committer
  { commit :: a -> IO Bool
  }

instance Semigroup (Committer a) where
  (<>) i1 i2 = Committer (\a -> (||) <$> commit i1 a <*> commit i2 a)

instance Monoid (Committer a) where
  mempty = Committer (\_ -> return False)
  mappend = (<>)

instance Contravariant Committer where
  contramap f (Committer a) = Committer (a . f)

instance Divisible Committer where
  conquer = Committer (\_ -> return False)
  divide f i1 i2 =
    Committer $ \a ->
      case f a of
        (b, c) -> (||) <$> commit i1 b <*> commit i2 c

instance Decidable Committer where
  lose f = Committer (absurd . f)
  choose f i1 i2 =
    Committer $ \a ->
      case f a of
        Left b -> commit i1 b
        Right c -> commit i2 c

-- | A Box is a product of a Committer and an Emitter. Think of a box with an incoming wire and an outgoing wire. Now notice that the abstraction is reversable: are you looking at two wires from "inside a box"; a blind erlang grunt communicating with the outside world via the two thin wires, or are you looking from "outside the box"; interacting with a black box object. Either way, it's a box.
-- And either way, the committer is contravariant and the emitter covariant so it forms a profunctor.
--
-- a Box can also be seen as having an input tape and output tape, thus available for turing and finite-state machine metaphorics.
--
data Box c e = Box
  { committer :: Committer c
  , emitter :: Emitter e
  }

instance Profunctor Box where
  dimap f g (Box c e) = Box (contramap f c) (fmap g e)

instance Semigroup (Box c e) where
  (<>) (Box c e) (Box c' e') = Box (c <> c') (e <> e')

instance Monoid (Box c e) where
  mempty = Box mempty mempty
  mappend = (<>)

-- | using fuse, it looks like Managed Boxes could form a Category, so here's a wrapper to try it out.
newtype BoxM a b =
  BoxM (Managed (Box a b))
  deriving (Monoid)

instance Semigroup (BoxM a b)

instance Profunctor BoxM where
  dimap f g (BoxM mbox) = BoxM
    (Box <$>
      (contramap f . committer <$> mbox) <*>
      (fmap g . emitter <$> mbox))

instance Category BoxM where
  (BoxM a) . (BoxM b) =
    BoxM $ do
      (Box c e) <- b
      (Box c' e') <- a
      liftIO $ fuse_ (pure . Just) e c'
      pure (Box c e')
  id =
    BoxM $ do
      (Box c e) <- toBox unbounded unbounded (const mempty) (const mempty)
      liftIO $ fuse_ (pure . Just) e c
      pure (Box c e)

-- | fuse an emitter directly to a committer
fuse_ :: (a -> IO (Maybe b)) -> Emitter a -> Committer b -> IO ()
fuse_ f e c = go where
  go = do
    a <- emit e
    fa <- maybe (pure Nothing) f a
    void $ maybe (pure False) (commit c) fa
    go

-- | fuse a box
--
-- >>> let committer' = cStdout 100 unbounded
-- >>> let emitter' = toEmit (bounded 1) (S.each ["hi","bye","q","x"])
-- >>> let box' = Box <$> committer' <*> emitter'
--
-- > fuse (pure . Just . ("echo: " <>)) box'
--
-- echo: hi
-- echo: bye
-- echo: q
-- echo: x
--
-- > fuse (pure . Just) $ Box <$> cStdout 2 (bounded 1) <*> emitter'
--
-- hi
-- bye
--
-- > etc () (Transducer id) == fuseBox (pure . Just)
--
fuse :: (a -> IO (Maybe b)) -> Managed (Box b a) -> IO ()
fuse f box = with box $ \(Box c e) -> fuse_ f e c

-- | fuse-branch an emitter
branchE :: Emitter a -> Committer a -> Emitter a
branchE e c = Emitter $
  do
    a <- emit e
    maybe (pure ()) (void <$> commit c) a
    pure a

-- | create a managed committer by fusing a committer
fuseCommit :: (b -> IO (Maybe a)) -> Buffer b -> Committer a -> Managed (Committer b)
fuseCommit f b c = managed $ \c' -> withBufferE b c' (\e -> fuse_ f e c)

-- | create a managed emmitter by fusing a emitter
fuseEmit :: (a -> IO (Maybe b)) -> Buffer b -> Emitter a -> Managed (Emitter b)
fuseEmit f b e = managed $ withBufferC b (fuse_ f e)

-- | alternate between two emitters
-- > let e1 = eStdin 4 unbounded
-- > let e2 = fmap show <$> (toEmit unbounded <| delayTimed (S.each (zip ((*2) . fromIntegral <$> [1..10]) [100..110]))) :: Managed (Emitter Text)
-- > let b1 = Box <$> cStdout 20 unbounded <*> alternate (pure . pure) e2 e1 :: Managed (Box Text Text)
-- > fuse (pure . Just) b1
-- > getEmissions 10 (alternate (pure . pure) (eStdin 4 (bounded 1)) ( fmap show <$> (toEmit unbounded <| delayTimed (S.each (zip ((*2) . fromIntegral <$> [1..10]) [100..110])))))
alternate :: (Show a) => (a -> IO (Maybe b)) ->
  Managed (Emitter a) -> Managed (Emitter a) -> Managed (Emitter b)
alternate f eL eR = managed $ \e ->
  with eL $ \eL' -> with eR $ \eR' -> withBufferC (bounded 1) (go eL' eR') e
  where
    go el er c = do
      void $ trye el er c
      go er el c

    trye e0 e1 c = do
      a <- emit e0
      case a of
        Nothing -> do
          a1 <- emit e1
          case a1 of
            Nothing -> pure False
            Just a' -> do
              print a'
              fa <- f a'
              case fa of
                Nothing -> pure False
                Just b -> commit c b
        Just a' -> do
          print a'
          fa <- f a'
          case fa of
            Nothing -> pure False
            Just b -> commit c b

-- | a modifier that feeds commits back to the emitter
feedback :: (b -> IO (Maybe a)) -> Managed (Box a b) -> Managed (Box a b)
feedback f box =
  managed $ \box' ->
    with box $ \(Box c e) -> do
      fuse_ f e c
      box' (Box c e)

-- | transduction
-- [wiki](https://en.wikipedia.org/wiki/Transducer) says: "A transducer is a device that converts energy from one form to another." Translated to context, this Transducer converts a stream of type a to a stream of a different type.
--
newtype Transducer s a b =
  Transducer
  { transduce ::
      forall m. Monad m =>
      Stream (Of a) (StateT s m) () -> Stream (Of b) (StateT s m) ()
  }

instance Category (Transducer s) where
  (Transducer t1) . (Transducer t2) = Transducer (t1 . t2)
  id = Transducer id

-- | emit - transduce - commit
--
-- >>> etc () transducer' box'
-- echo: hi
-- echo: bye
--
-- with etc, you're in the box, and inside the box, there are no effects: just a stream of 'a's, pure functions and state tracking. It's a nice way to code, and very friendly for the compiler. When the committing and emitting is done, the box collapses to state.
--
-- The combination of an input tape, an output tape, and a state-based stream computation lends itself to the etc computation as a [finite-state transducer](https://en.wikipedia.org/wiki/Finite-state_transducer) or mealy machine.
--
etc :: s -> Transducer s a b -> Managed (Box b a) -> IO s
etc st t box =
  with box $ \(Box c e) ->
    (e |> toStream |> transduce t |> fromStream) c |> flip execStateT st

-- | turn an emitter into a stream
toStream :: (MonadBase IO m) => Emitter a -> Stream (Of a) m ()
toStream e = S.untilRight getNext
  where
    getNext = maybe (Right ()) Left <$> liftBase (emit e)

-- | turn a stream into a committer
fromStream :: (MonadBase IO m) => Stream (Of b) m () -> Committer b -> m ()
fromStream s c = go s
  where
    go str = do
      eNxt <- S.next str -- uncons requires r ~ ()
      forM_ eNxt $ \(a, str') -> do
        continue <- liftBase (commit c a)
        when continue (go str')

-- * concurrency

-- | copied shamefully from pipes-concurrency
spawn' :: Buffer a -> IO (Committer a, Emitter a, STM ())
spawn' buffer = do
  (write, read) <-
    case buffer of
      Bounded n -> do
        q <- S.newTBQueueIO n
        return (S.writeTBQueue q, S.readTBQueue q)
      Unbounded -> do
        q <- S.newTQueueIO
        return (S.writeTQueue q, S.readTQueue q)
      Single -> do
        m <- S.newEmptyTMVarIO
        return (S.putTMVar m, S.takeTMVar m)
      Latest a -> do
        t <- S.newTVarIO a
        return (S.writeTVar t, S.readTVar t)
      New -> do
        m <- S.newEmptyTMVarIO
        return (\x -> S.tryTakeTMVar m *> S.putTMVar m x, S.takeTMVar m)
      Newest n -> do
        q <- S.newTBQueueIO n
        let write x = S.writeTBQueue q x <|> (S.tryReadTBQueue q *> write x)
        return (write, S.readTBQueue q)
  sealed <- S.newTVarIO False
  let seal = S.writeTVar sealed True
    {- Use weak TVars to keep track of whether the 'Input' or 'Output' has been
       garbage collected.  Seal the mailbox when either of them becomes garbage
       collected.
    -}
  rSend <- S.newTVarIO ()
  void $ S.mkWeakTVar rSend (S.atomically seal)
  rRecv <- S.newTVarIO ()
  void $ S.mkWeakTVar rRecv (S.atomically seal)
  let sendOrEnd a = do
        b <- S.readTVar sealed
        if b
          then return False
          else do
            write a
            return True
      readOrEnd =
        (Just <$> read) <|>
        (do b <- S.readTVar sealed
            S.check b
            return Nothing)
      _send a = sendOrEnd a <* S.readTVar rSend
      _recv = readOrEnd <* S.readTVar rRecv
  return (Committer (atomically . _send), Emitter (atomically _recv), seal)

-- | wait for the first action, and then cancel the second
waitCancel :: IO b -> IO a -> IO b
waitCancel a b =
  withAsync a $ \a' ->
    withAsync b $ \b' -> do
      a'' <- wait a'
      cancel b'
      pure a''

-- | connect a committer and emitter action via a buffer, and wait for both to complete.
withBuffer ::
     Buffer a -> (Committer a -> IO l) -> (Emitter a -> IO r) -> IO (l, r)
withBuffer buffer fOutput fInput =
  bracket
    (spawn' buffer)
    (\(_, _, seal) -> atomically seal)
    (\(output, input, seal) ->
       Control.Concurrent.Async.concurrently
         (fOutput output `Control.Exception.finally` atomically seal)
         (fInput input `Control.Exception.finally` atomically seal))

-- | connect a committer and emitter action via a buffer, cancelling the emitter on commision completion (Emitter biased).
withBufferE :: Buffer a -> (Committer a -> IO l) -> (Emitter a -> IO r) -> IO l
withBufferE buffer fOutput fInput =
  bracket
    (spawn' buffer)
    (\(_, _, seal) -> atomically seal)
    (\(output, input, seal) ->
       waitCancel
         (fOutput output `Control.Exception.finally` atomically seal)
         (fInput input `Control.Exception.finally` atomically seal))

-- | connect a committer and emitter action via a buffer, cancelling the committer on emission completion. (Committer biased)
withBufferC :: Buffer a -> (Committer a -> IO l) -> (Emitter a -> IO r) -> IO r
withBufferC buffer fOutput fInput =
  bracket
    (spawn' buffer)
    (\(_, _, seal) -> atomically seal)
    (\(output, input, seal) ->
       waitCancel
         (fInput input `Control.Exception.finally` atomically seal)
         (fOutput output `Control.Exception.finally` atomically seal))

-- | 'Buffer' specifies how to buffer messages stored within the mailbox
data Buffer a
  = Unbounded
  | Bounded Int
  | Single
  | Latest a
  | Newest Int
  | New

-- | Store an unbounded number of messages in a FIFO queue
unbounded :: Buffer a
unbounded = Unbounded

-- | Store a bounded number of messages, specified by the 'Int' argument
bounded :: Int -> Buffer a
bounded 1 = Single
bounded n = Bounded n

{-| Only store the 'Latest' message, beginning with an initial value

    'Latest' is never empty nor full.
-}
latest :: a -> Buffer a
latest = Latest

{-| Like @Bounded@, but 'send' never fails (the buffer is never full).
    Instead, old elements are discarded to make room for new elements
-}
newest :: Int -> Buffer a
newest 1 = New
newest n = Newest n


-- * boxing

-- | hook an emitter to a buffer, creating a managed committer
buffCommit :: Buffer a -> (Emitter a -> IO ()) -> Managed (Committer a)
buffCommit b e = managed $ \c -> withBufferE b c e

-- | hook a committer to a buffer, creating a managed emitter
buffEmit :: Buffer a -> (Committer a -> IO r) -> Managed (Emitter a)
buffEmit b c = managed $ \e -> withBufferC b c e

-- | create a managed, double-buffered box hooking up emitter and committer actions
toBox ::
     Buffer a
  -> Buffer b
  -> (Emitter a -> IO ())
  -> (Committer b -> IO ())
  -> Managed (Box a b)
toBox ba bb eio cio = Box <$> buffCommit ba eio <*> buffEmit bb cio

-- | create a managed box from a box action, forgetting the interactions between emitter and committer that typically gum up the works
toBoxForget :: Buffer a -> Buffer b -> (Box b a -> IO ()) -> Managed (Box a b)
toBoxForget ba bb bio =
  let eio = bio . Box mempty
      cio = bio . (`Box` mempty)
  in toBox ba bb eio cio

-- * streaming
-- | create a committer from a stream consumer
toCommit :: Buffer a -> (Stream (Of a) IO () -> IO r) -> Managed (Committer a)
toCommit b f = managed (\c -> withBufferE b c (toStream .> f))

-- | create a committer from a fold
toCommitFold :: Buffer a -> L.FoldM IO a () -> Managed (Committer a)
toCommitFold b f = toCommit b (L.impurely S.foldM f .> fmap S.snd')

-- | create a committer from an IO sink
toCommitIO :: Buffer a -> (a -> IO ()) -> Managed (Committer a)
toCommitIO b sink = toCommitFold b (L.FoldM step begin done)
  where
    step x a = do
      sink a
      pure x
    begin = pure ()
    done = pure

-- | create an emitter from a stream
toEmit :: Buffer a -> Stream (Of a) IO () -> Managed (Emitter a)
toEmit b s = managed (withBufferC b (fromStream s))

-- | insert a buffer into a stream (Committer biased)
buffC ::
     Buffer a -> Stream (Of a) IO () -> (Stream (Of a) IO () -> IO r) -> IO r
buffC b i o = withBufferC b (fromStream i) (toStream .> o)

-- | insert a buffer into a stream (Emitter biased)
buffE ::
     Buffer a -> Stream (Of a) IO () -> (Stream (Of a) IO () -> IO r) -> IO ()
buffE b i o = withBufferE b (fromStream i) (toStream .> o)


-- * console
-- | a single stdin committer action
cStdin_ :: Committer Text -> IO ()
cStdin_ c = getLine >>= commit c |> void

-- | a finite stdin committer action
cStdin :: Int -> Committer Text -> IO ()
cStdin n c = replicateM_ n (cStdin_ c)

-- | a forever stdin committer action
cStdin' :: Committer Text -> IO ()
cStdin' = forever . cStdin_

-- | a managed stdin emitter
eStdin :: Int -> Buffer Text -> Managed (Emitter Text)
eStdin n b = cStdin n |> buffEmit b

-- | read from console
readStdin :: Read a => Managed (Emitter a)
readStdin = eRead (eStdin 1000 unbounded)

-- | a single stdout emitter action
eStdout_ :: (Print a) => Emitter a -> IO ()
eStdout_ e = do
  a <- emit e
  case a of
    Nothing -> pure ()
    Just a' -> putStrLn a'

-- | a finite stdout emitter action
eStdout :: (Print a) => Int -> Emitter a -> IO ()
eStdout n = replicateM_ n . eStdout_

-- | a forever stdout emitter action
eStdout' :: (Print a) => Emitter a -> IO ()
eStdout' = forever . eStdout_

-- | a managed stdout committer
cStdout :: Print a => Int -> Buffer a -> Managed (Committer a)
cStdout n b = eStdout n |> buffCommit b

-- | show to stdout
showStdout :: Show a => Managed (Committer a)
showStdout =
  contramap show <$> (cStdout 1000 unbounded :: Managed (Committer Text))

-- | console box
-- > etc () (Trans $ \s -> s & S.takeWhile (/="q") & S.map ("echo: " <>)) (console 5)
console :: Int -> Buffer Text -> Managed (Box Text Text)
console n b = toBox b b (eStdout n) (cStdin n)

-- * file operations
-- | emit lines from a file
emitLines :: FilePath -> Buffer Text -> Managed (Emitter Text)
emitLines filePath b =
  managed (withFile filePath ReadMode) >>= fromHandle .> toEmit b
  where
    fromHandle :: Handle -> Stream (Of Text) IO ()
    fromHandle h =
      forever $ do
        a <- liftIO $ Text.hGetLine h
        S.yield a

-- | commit lines to a file
commitLines :: FilePath -> Buffer Text -> Managed (Committer Text)
commitLines filePath b =
  managed (withFile filePath WriteMode) >>= toHandle .> toCommit b
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
cIORef :: IORef [b] -> Buffer b -> Managed (Committer b)
cIORef ref b =
  toCommitFold b $
  L.FoldM (\x a -> modifyIORef x (a :) >> pure x) (pure ref) (const $ pure ())

-- | commit to a monoidal IORef
cIORefM :: Semigroup m => IORef m -> Buffer m -> Managed (Committer m)
cIORefM ref b =
  toCommitFold b $
  L.FoldM (\x a -> modifyIORef x (a <>) >> pure x) (pure ref) (const $ pure ())

-- | fold an emitter through a transduction, committing to a list
toListIO ::
     Buffer b -> Managed (Emitter a) -> s -> Transducer s a b -> IO ([b], s)
toListIO b e s t = do
  ref <- newIORef []
  r <- etc s t (Box <$> cIORef ref b <*> e)
  (,) <$> readIORef ref <*> pure r

-- | get all commissions as a list
getCommissions :: Managed (Emitter a) -> s -> Transducer s a b -> IO [b]
getCommissions e s t = fst <$> toListIO unbounded e s t

-- | get all emissions
getEmissions :: Int -> Managed (Emitter a) -> IO [a]
getEmissions n e = fst <$> toListIO unbounded e () (Transducer $ S.take n)

-- | keep valid emissions
keeps ::
     ((b -> Constant (First b) b) -> (a -> Constant (First b) a))
    -- ^
  -> Emitter a
    -- ^
  -> Emitter b
keeps k (Emitter emit_) = Emitter emit_'
  where
    emit_' = do
      ma <- emit_
      case ma of
        Nothing -> return Nothing
        Just a ->
          case match a of
            Nothing -> emit_'
            Just b -> return (Just b)
    match = getFirst . getConstant . k (Constant . First . Just)

-- | parsing, throwing away errors
eParse :: A.Parser a -> Managed (Emitter Text) -> Managed (Emitter a)
eParse parser e = keeps _Right . fmap (A.parseOnly parser) <$> e

-- | read an a, throwing away errors
eRead :: (Read a) => Managed (Emitter Text) -> Managed (Emitter a)
eRead = fmap (keeps parsed) . fmap (fmap Text.unpack)
  where
    parsed k str =
      case reads str of
        [(a, "")] -> Constant (getConstant (k a))
        _ -> pure str

eRead' :: (Read a) => Managed (Emitter Text) -> Managed (Emitter (Either Text a))
eRead' = fmap (fmap $ parsed . Text.unpack)
  where
    parsed str =
      case reads str of
        [(a, "")] -> Right a
        _ -> Left (Text.pack str)

-- | show an a
cShow :: (Show a) => Managed (Committer Text) -> Managed (Committer a)
cShow c = contramap show <$> c

-- | handle certain commissions
handles ::
     ((b -> Constant (First b) b) -> (a -> Constant (First b) a))
    -- ^
  -> Committer b
    -- ^
  -> Committer a
handles k (Committer commit_) =
  Committer
    (\a ->
       case match a of
         Nothing -> return False
         Just b -> commit_ b)
  where
    match = getFirst . getConstant . k (Constant . First . Just)

-- | convert a Pipe to a Transducer
asPipe ::
     (Monad m)
  => Pipes.Pipe a b (StateT s m) ()
  -> (Stream (Of a) (StateT s m) () -> Stream (Of b) (StateT s m) ())
asPipe p s = ((s & Pipes.unfoldr S.next) Pipes.>-> p) & S.unfoldr Pipes.next
