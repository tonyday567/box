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
  ( Emitter(..)
  , Committer(..)
  , Box(..)
  -- , BoxM(..)
  , safeIOToSTM
  , emitIO
  , fuse_
  , fuse
  , forkEmit
  , Transducer(..)
  , etc
  , toStream
  , fromStream
  , withBuffer
  , withBufferE
  , withBufferC
  , withBuffer'
  , Buffer(..)
  , bounded
  , unbounded
  , latest
  , newest
  , buffCommit
  , buffEmit
  , buffBox
  , buffBoxForget
  , toCommit
  , toCommitFold
  , toCommitIO
  , toEmit
  , buffC
  , buffE
  , fuseEmit
  , fuseCommit
  , merge
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
  , feedbackE
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
import Etc.Managed
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
import GHC.Conc 

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
-- >>> with (S.each [0..] & toEmit unbounded) (atomically . emit) >>= print
-- Just 0
--
newtype Emitter m a = Emitter
  { emit :: m (Maybe a)
  }

instance (Functor m) => Functor (Emitter m) where
  fmap f m = Emitter (fmap (fmap f) (emit m))

instance (Applicative m) => Applicative (Emitter m) where
  pure r = Emitter (pure (pure r))
  mf <*> mx = Emitter ((<*>) <$> emit mf <*> emit mx)

instance (Monad m) => Monad (Emitter m) where
  return r = Emitter (return (return r))
  m >>= f =
    Emitter $ do
      ma <- emit m
      case ma of
        Nothing -> return Nothing
        Just a -> emit (f a)

instance (Monad m, Alternative m) => Alternative (Emitter m) where
  empty = Emitter (pure Nothing)
  x <|> y =
    Emitter $ do
      (i, ma) <- fmap ((,) y) (emit x) <|> fmap ((,) x) (emit y)
      case ma of
        Nothing -> emit i
        Just a -> return (Just a)

instance (Alternative m, Monad m) => MonadPlus (Emitter m) where
  mzero = empty
  mplus = (<|>)

instance (Alternative m, Monad m) => Semigroup (Emitter m a) where
  (<>) = (<|>)

instance (Alternative m, Monad m) => Monoid (Emitter m a) where
  mempty = empty
  mappend = (<>)

-- | a Committer commits values of type a (to the effects void presumably). A Sink for 'a's & a Consumer of 'a's are the other metaphors. It is a newtype wrapper around pipes-concurrency Output to pick up the instances.
--
-- Naming is reversed in comparison to the 'Output' it wraps.  An Committer 'reaches out and absorbs' the value being committed; the value disappears into the opaque thing that is a Committer from the pov of usage.  An Committer is named for its' main action: it commits.
--
-- >>> import Etc.Time
-- >>> with (cStdout 100 unbounded) $ \c -> atomically (commit c "something") >> sleep 1
-- something
--
newtype Committer m a = Committer
  { commit :: a -> m Bool
  }

instance (Applicative m) => Semigroup (Committer m a) where
  (<>) i1 i2 = Committer (\a -> (||) <$> commit i1 a <*> commit i2 a)

instance (Applicative m) => Monoid (Committer m a) where
  mempty = Committer (\_ -> pure False)
  mappend = (<>)

instance Contravariant (Committer m) where
  contramap f (Committer a) = Committer (a . f)

instance (Applicative m) => Divisible (Committer m) where
  conquer = Committer (\_ -> pure False)
  divide f i1 i2 =
    Committer $ \a ->
      case f a of
        (b, c) -> (||) <$> commit i1 b <*> commit i2 c

instance (Applicative m) => Decidable (Committer m) where
  lose f = Committer (absurd . f)
  choose f i1 i2 =
    Committer $ \a ->
      case f a of
        Left b -> commit i1 b
        Right c -> commit i2 c

-- | A Box is a product of a Committer m and an Emitter. Think of a box with an incoming wire and an outgoing wire. Now notice that the abstraction is reversable: are you looking at two wires from "inside a box"; a blind erlang grunt communicating with the outside world via the two thin wires, or are you looking from "outside the box"; interacting with a black box object. Either way, it's a box.
-- And either way, the committer is contravariant and the emitter covariant so it forms a profunctor.
--
-- a Box can also be seen as having an input tape and output tape, thus available for turing and finite-state machine metaphorics.
--
data Box m c e = Box
  { committer :: Committer m c
  , emitter :: Emitter m e
  }

instance (Functor m) => Profunctor (Box m) where
  dimap f g (Box c e) = Box (contramap f c) (fmap g e)

instance (Alternative m, Monad m) => Semigroup (Box m c e) where
  (<>) (Box c e) (Box c' e') = Box (c <> c') (e <> e')

instance (Alternative m, Monad m) => Monoid (Box m c e) where
  mempty = Box mempty mempty
  mappend = (<>)

{-
-- | using fuse, it looks like Managed Boxes could form a Category, so here's a wrapper to try it out.

newtype BoxM m a b =
  BoxM (Managed m (Box m a b))
  deriving (Monoid)

instance (Alternative m, Monad m) => Semigroup (BoxM m a b)

instance (Functor m) => Profunctor (BoxM m) where
  dimap f g (BoxM mbox) = BoxM
    (Box <$>
      (contramap f . committer <$> mbox) <*>
      (fmap g . emitter <$> mbox))

instance Category (BoxM m) where
  (BoxM a) . (BoxM b) =
    BoxM $ do
      (Box c e) <- b
      (Box c' e') <- a
      lift $ fuse_ (pure . Just) e c'
      pure (Box c e')
  id =
    BoxM $ do
      (Box c e) <- toBox unbounded unbounded (const mempty) (const mempty)
      liftIO $ fuse_ (pure . Just) e c
      pure (Box c e)
-}

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

-- | convert a Pipe to a Transducer
asPipe ::
     (Monad m)
  => Pipes.Pipe a b (StateT s m) ()
  -> (Stream (Of a) (StateT s m) () -> Stream (Of b) (StateT s m) ())
asPipe p s = ((s & Pipes.unfoldr S.next) Pipes.>-> p) & S.unfoldr Pipes.next

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
etc :: s -> Transducer s a b -> Managed IO (Box STM b a) -> IO s
etc st t box =
  with box $ \(Box c e) ->
    (e |> toStreamIO |> transduce t |> fromStreamIO) c |> flip execStateT st

-- | turn an emitter into a stream
toStream :: (MonadBase STM m) => Emitter STM a -> Stream (Of a) m ()
toStream e = S.untilRight getNext
  where
    getNext = maybe (Right ()) Left <$> liftBase (emit e)

-- | turn an emitter into a stream
toStreamIO :: (MonadBase IO m) => Emitter STM a -> Stream (Of a) m ()
toStreamIO e = S.untilRight getNext
  where
    getNext = maybe (Right ()) Left <$> liftBase (atomically (emit e))

-- | turn a stream into a committer
fromStream :: (MonadBase STM m) => Stream (Of b) m () -> Committer STM b -> m ()
fromStream s c = go s
  where
    go str = do
      eNxt <- S.next str -- uncons requires r ~ ()
      forM_ eNxt $ \(a, str') -> do
        continue <- liftBase $ commit c a
        when continue (go str')

-- | turn a stream into a committer
fromStreamIO :: (MonadBase IO m) => Stream (Of b) m () -> Committer STM b -> m ()
fromStreamIO s c = go s
  where
    go str = do
      eNxt <- S.next str -- uncons requires r ~ ()
      forM_ eNxt $ \(a, str') -> do
        continue <- liftBase $ atomically $ commit c a
        when continue (go str')

-- * concurrency

-- | copied shamefully from pipes-concurrency
spawn' :: Buffer a -> IO (Committer STM a, Emitter STM a, STM ())
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
  return (Committer _send, Emitter _recv, seal)

safeIOToSTM :: IO a -> STM a
safeIOToSTM req = unsafeIOToSTM $ do
  tv <- newEmptyMVar
  _ <- forkIO $ ((putMVar tv . Right) =<< req)
          `Control.Exception.catch`
            (\(e :: SomeException) -> putMVar tv $ Left e )
  r <-  takeMVar tv
  case r of
    Right x -> return x
    Left e -> Control.Exception.throw e

-- * combining IO and STM

-- | transform an Emitter with an action
emitIO :: (a -> IO (Maybe b)) -> Emitter STM a -> Emitter STM b
emitIO f e = Emitter go where
  go = do
    a <- emit e
    case a of
      Nothing -> pure Nothing
      Just a' -> do
        fa <- safeIOToSTM $ f a'
        case fa of
          Nothing -> go
          Just fa' -> pure (Just fa')

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
     Buffer a -> (Committer STM a -> IO l) -> (Emitter STM a -> IO r) -> IO (l, r)
withBuffer buffer fOutput fInput =
  bracket
    (spawn' buffer)
    (\(_, _, seal) -> atomically seal)
    (\(output, input, seal) ->
       Control.Concurrent.Async.concurrently
         (fOutput output `Control.Exception.finally` atomically seal)
         (fInput input `Control.Exception.finally` atomically seal))

-- | connect a committer and emitter action via a buffer, cancelling the emitter on commision completion (Emitter m biased).
withBufferE :: Buffer a -> (Committer STM a -> IO l) -> (Emitter STM a -> IO r) -> IO l
withBufferE buffer fOutput fInput =
  bracket
    (spawn' buffer)
    (\(_, _, seal) -> atomically seal)
    (\(output, input, seal) ->
       waitCancel
         (fOutput output `Control.Exception.finally` atomically seal)
         (fInput input `Control.Exception.finally` atomically seal))

-- | connect a committer and emitter action via a buffer, cancelling the committer on emission completion. (Committer m biased)
withBufferC :: Buffer a -> (Committer STM a -> IO l) -> (Emitter STM a -> IO r) -> IO r
withBufferC buffer fOutput fInput =
  bracket
    (spawn' buffer)
    (\(_, _, seal) -> atomically seal)
    (\(output, input, seal) ->
       waitCancel
         (fInput input `Control.Exception.finally` atomically seal)
         (fOutput output `Control.Exception.finally` atomically seal))

-- | connect a committer and emitter action via a buffer, cancelling the committer on either action completion. (Committer m biased)
withBuffer' :: Buffer a -> (Committer STM a -> IO l) -> (Emitter STM a -> IO r) -> IO (Either r l)
withBuffer' buffer fOutput fInput =
  bracket
    (spawn' buffer)
    (\(_, _, seal) -> atomically seal)
    (\(output, input, seal) ->
       race
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


-- * primitives
-- | fuse an emitter directly to a committer
fuse_ :: (a -> IO (Maybe b)) -> Emitter STM a -> Committer STM b -> IO ()
fuse_ f e c = go where
  go = do
    b <- atomically $ do
      a <- emit e
      fa <- maybe (pure Nothing) (safeIOToSTM . f) a
      maybe (pure False) (commit c) fa
    when b go

-- | fuse a box
--
-- >>> let committer' = cStdout 100 unbounded
-- >>> let emitter' = toEmit (bounded 1) (S.each ["hi","bye","q","x"])
-- >>> let box' = Box <$> committer' <*> emitter'
--
-- >>> fuse (pure . Just . ("echo: " <>)) box'
-- echo: hi
-- echo: bye
-- echo: q
-- echo: x
--
-- >>> fuse (pure . Just) $ Box <$> cStdout 2 (bounded 1) <*> emitter'
-- hi
-- bye
--
-- > etc () (Transducer id) == fuse (pure . pure)
--
fuse :: (a -> IO (Maybe b)) -> Managed IO (Box STM b a) -> IO ()
fuse f box = with box $ \(Box c e) -> fuse_ f e c

-- | fuse-branch an emitter
forkEmit :: (Monad m) => Emitter m a -> Committer m a -> Emitter m a
forkEmit e c = Emitter $
  do
    a <- emit e
    maybe (pure ()) (void <$> commit c) a
    pure a

-- * buffer hookups
-- | hook an emitter action to a buffer, creating a managed committer
buffCommit :: Buffer a -> (Emitter STM a -> IO ()) -> Managed IO (Committer STM a)
buffCommit b eio = managed $ \cio -> withBufferE b cio eio

-- | hook a committer action to a buffer, creating a managed emitter
buffEmit :: Buffer a -> (Committer STM a -> IO r) -> Managed IO (Emitter STM a)
buffEmit b cio = managed $ \eio -> withBufferC b cio eio

-- | create a managed, double-buffered box hooking up emitter and committer actions
buffBox ::
     Buffer a
  -> Buffer b
  -> (Emitter STM a -> IO ())
  -> (Committer STM b -> IO ())
  -> Managed IO (Box STM a b)
buffBox bc be eio cio = Box <$> buffCommit bc eio <*> buffEmit be cio

-- | create a managed box from a box action.  Caution: implicitly, this (has to) forget interactions between emitter and committer in the one action (and it does so silently).  These forgotten interactions are typically those that create races
buffBoxForget :: Buffer a -> Buffer b -> (Box STM b a -> IO ()) -> Managed IO (Box STM a b)
buffBoxForget ba bb bio =
  let eio = bio . Box mempty
      cio = bio . (`Box` mempty)
  in buffBox ba bb eio cio

-- | fuse a committer to a buffer, with a transformation
fuseCommit :: Buffer b -> (b -> IO (Maybe a)) -> Committer STM a -> Managed IO (Committer STM b)
fuseCommit b f c = managed $ \cio -> withBufferE b cio (\e -> fuse_ f e c)

-- | fuse an emitter to a buffer, with a transformation
fuseEmit :: Buffer b -> (a -> IO (Maybe b)) -> Emitter STM a -> Managed IO (Emitter STM b)
fuseEmit b f e = managed $ \eio -> withBufferC b (fuse_ f e) eio

-- | merge two emitters
--
-- This differs from `liftA2 (<>)` in that the monoidal (and alternative) instance of an Emitter is left-biased (The left emitter exhausts before the right one is begun). This merge is concurrent.
--
-- >>> import Etc.Time (delayTimed)
-- >>> let e1 = fmap show <$> (toEmit unbounded <| delayTimed (S.each (zip (fromIntegral <$> [1..10]) ['a'..]))) :: Managed IO (Emitter STM Text)
-- >>> let e2 = fmap show <$> (toEmit unbounded <| delayTimed (S.each (zip ((\x -> fromIntegral x + 0.1) <$> [1..10]) (reverse ['a'..'z'])))) :: Managed IO (Emitter STM Text)
-- >>> let b = Box <$> cStdout 6 unbounded <*> merge unbounded unbounded (pure . pure) e1 e2
-- >>> etc () (Transducer identity) b
-- 'a'
-- 'z'
-- 'b'
-- 'y'
-- 'c'
-- 'x'
--
merge :: Buffer b -> Buffer b -> (a -> IO (Maybe b)) ->
  Managed IO (Emitter STM a) -> Managed IO (Emitter STM a) -> Managed IO (Emitter STM b)
merge bL bR f eL eR = managed $ \e ->
  with eL $ \eL' ->
  with eR $ \eR' ->
  fst <$> concurrently
    (withBufferC bL (fuse_ f eL') e)
    (withBufferC bR (fuse_ f eR') e)

-- | a box modifier that feeds commits back to the emitter
-- feedback :: (a -> IO (Maybe b)) -> Managed IO (Box STM a b) -> Managed IO (Box STM a b)

-- | an emitter post-processor that cons transformed emissions back into the emitter
feedbackE :: (a -> IO (Maybe a)) -> Emitter STM a -> Managed IO (Emitter STM a)
feedbackE f e =
      merge unbounded unbounded (pure . pure) (pure e) (fuseEmit unbounded f e)

-- * streaming
-- | create a committer from a stream consumer
toCommit :: Buffer a -> (Stream (Of a) IO () -> IO r) -> Managed IO (Committer STM a)
toCommit b f = managed (\c -> withBufferE b c (\(Emitter o) -> f . toStreamIO . Emitter $ o))

-- | create a committer from a fold
toCommitFold :: Buffer a -> L.FoldM IO a () -> Managed IO (Committer STM a)
toCommitFold b f = toCommit b (L.impurely S.foldM f .> fmap S.snd')

-- | create a committer from an IO sink
toCommitIO :: Buffer a -> (a -> IO ()) -> Managed IO (Committer STM a)
toCommitIO b sink = toCommitFold b (L.FoldM step begin done)
  where
    step x a = do
      sink a
      pure x
    begin = pure ()
    done = pure

-- | create an emitter from a stream
toEmit :: Buffer a -> Stream (Of a) IO () -> Managed IO (Emitter STM a)
toEmit b s = managed (withBufferC b (fromStreamIO s))

-- | insert a buffer into a stream (Committer m biased)
buffC ::
     Buffer a -> Stream (Of a) IO () -> (Stream (Of a) IO () -> IO r) -> IO r
buffC b i o = withBufferC b (fromStreamIO i) (toStreamIO .> o)

-- | insert a buffer into a stream (Emitter m biased)
buffE ::
     Buffer a -> Stream (Of a) IO () -> (Stream (Of a) IO () -> IO r) -> IO ()
buffE b i o = withBufferE b (fromStreamIO i) (toStreamIO .> o)

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

-- | a managed stdin emitter
eStdin :: Int -> Buffer Text -> Managed IO (Emitter STM Text)
eStdin n b = cStdin n |> buffEmit b

-- | read from console
readStdin :: Read a => Managed IO (Emitter STM a)
readStdin = eRead (eStdin 1000 unbounded)

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

-- | a managed stdout committer
cStdout :: Print a => Int -> Buffer a -> Managed IO (Committer STM a)
cStdout n b = eStdout n |> buffCommit b

-- | show to stdout
showStdout :: Show a => Managed IO (Committer STM a)
showStdout =
  contramap show <$> (cStdout 1000 unbounded :: Managed IO (Committer STM Text))

-- | console box
-- > etc () (Trans $ \s -> s & S.takeWhile (/="q") & S.map ("echo: " <>)) (console 5)
console :: Int -> Buffer Text -> Managed IO (Box STM Text Text)
console n b = buffBox b b (eStdout n) (cStdin n)

-- * file operations
-- | emit lines from a file
emitLines :: FilePath -> Buffer Text -> Managed IO (Emitter STM Text)
emitLines filePath b =
  managed (withFile filePath ReadMode) >>= fromHandle .> toEmit b
  where
    fromHandle :: Handle -> Stream (Of Text) IO ()
    fromHandle h =
      forever $ do
        a <- liftIO $ Text.hGetLine h
        S.yield a

-- | commit lines to a file
commitLines :: FilePath -> Buffer Text -> Managed IO (Committer STM Text)
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
cIORef :: IORef [b] -> Buffer b -> Managed IO (Committer STM b)
cIORef ref b =
  toCommitFold b $
  L.FoldM (\x a -> modifyIORef x (a :) >> pure x) (pure ref) (const $ pure ())

-- | commit to a monoidal IORef
cIORefM :: Semigroup a => IORef a -> Buffer a -> Managed IO (Committer STM a)
cIORefM ref b =
  toCommitFold b $
  L.FoldM (\x a -> modifyIORef x (a <>) >> pure x) (pure ref) (const $ pure ())

-- | fold an emitter through a transduction, committing to a list
toListIO ::
     Buffer b -> Managed IO (Emitter STM a) -> s -> Transducer s a b -> IO ([b], s)
toListIO b e s t = do
  ref <- newIORef []
  r <- etc s t (Box <$> cIORef ref b <*> e)
  (,) <$> readIORef ref <*> pure r

-- | get all commissions as a list
getCommissions :: Managed IO (Emitter STM a) -> s -> Transducer s a b -> IO [b]
getCommissions e s t = fst <$> toListIO unbounded e s t

-- | get all emissions
getEmissions :: Int -> Managed IO (Emitter STM a) -> IO [a]
getEmissions n e = fst <$> toListIO unbounded e () (Transducer $ S.take n)

-- | keep valid emissions
keeps :: (Monad m) =>
     ((b -> Constant (First b) b) -> (a -> Constant (First b) a))
    -- ^
  -> Emitter m a
    -- ^
  -> Emitter m b
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
eParse :: A.Parser a -> Managed IO (Emitter STM Text) -> Managed IO (Emitter STM a)
eParse parser e = keeps _Right . fmap (A.parseOnly parser) <$> e

-- | read an a, throwing away errors
eRead :: (Read a) => Managed IO (Emitter STM Text) -> Managed IO (Emitter STM a)
eRead = fmap (keeps parsed) . fmap (fmap Text.unpack)
  where
    parsed k str =
      case reads str of
        [(a, "")] -> Constant (getConstant (k a))
        _ -> pure str

eRead' :: (Read a) => Managed IO (Emitter STM Text) -> Managed IO (Emitter STM (Either Text a))
eRead' = fmap (fmap $ parsed . Text.unpack)
  where
    parsed str =
      case reads str of
        [(a, "")] -> Right a
        _ -> Left (Text.pack str)

-- | show an a
cShow :: (Show a) => Managed IO (Committer STM Text) -> Managed IO (Committer STM a)
cShow c = contramap show <$> c

-- | handle certain commissions
handles :: (Monad m) =>
     ((b -> Constant (First b) b) -> (a -> Constant (First b) a))
    -- ^
  -> Committer m b
    -- ^
  -> Committer m a
handles k (Committer commit_) =
  Committer
    (\a ->
       case match a of
         Nothing -> return False
         Just b -> commit_ b)
  where
    match = getFirst . getConstant . k (Constant . First . Just)

