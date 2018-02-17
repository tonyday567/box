{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | emit transduce commit
--
-- like MVC but with more pretentious names
--
module Etc where

import Data.Functor.Constant
import Control.Category
import Control.Lens hiding ((|>), (.>), (<.), (:>))
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Managed
import Data.Functor.Contravariant.Divisible
import Data.IORef
import Data.Semigroup hiding (First, getFirst)
import Flow
import Pipes.Concurrent as P
import Protolude hiding ((.), (<>))
import Streaming (Of(..), Stream)
import qualified Control.Foldl as L
import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import qualified Streaming.Internal as S
import qualified Streaming.Prelude as S
-- import qualified Streaming as S
import qualified Data.Attoparsec.Text as A
import Text.Read (reads)
import qualified Pipes
import qualified Pipes.Prelude as Pipes

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> let b = Box <$> cStdout 2 (bounded 1) <*> toEmit (bounded 1) (S.each ["hi","bye","q","x"]) :: Managed (Box Text Text)
-- >>> let t = Transducer $ \s -> s & S.takeWhile (/="q") & S.map ("echo: " <>)
--

-- | an Emitter emits values of type a. A Source of 'a's & a Producer of 'a's are the two obvious alternative but overloaded metaphors out there. It is a newtype warpper around pipes-concurrency Input to pick up the instances.
newtype Emitter a =
  Emitter (Input a)
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, Monoid)

instance Semigroup (Emitter a)

-- | maybe emit a value from an emitter
emit' :: Emitter a -> IO (Maybe a)
emit' (Emitter i) = atomically $ recv i

-- | emit from an emitter
-- >  with b $ \b' -> (b' |> emitter |> emit) >>= putStrLn
-- hi
--
emit :: Emitter a -> IO a
emit (Emitter i) = do
  a <- atomically $ recv i
  case a of
    Nothing -> emit (Emitter i)
    Just a' -> pure a'

-- | a Committer commits values of type a (to the effects void presumably). A Sink for 'a's & a Consumer of 'a's are the other metaphors. It is a newtype wrapper around pipes-concurrency Output to pick up the instances.
newtype Committer a =
  Committer (Output a)
  deriving (Monoid, Contravariant, Divisible, Decidable)

instance Semigroup (Committer a)

-- | commit to a committer
-- > with b $ \b' -> commit (committer b') "something"
-- something
--
commit :: Committer a -> a -> IO ()
commit (Committer o) = void . atomically . send o

-- | commit to a committer, returning a flag for success
--
commit' :: Committer a -> a -> IO Bool
commit' (Committer o) = atomically . send o

-- | A Box is a product of a Committer and an Emitter. Think of a box with an incoming wire and an outgoing wire. Now notice that the abstraction is reversable: are you looking at two wires from "inside a box"; a blind erlang grunt communicating with the outside world via the two thin wires, or are you looking from "outside the box"; interacting with a black box object. Either way, it's a box.
-- And either way, the committer is contravariant and the emitter covariant so it forms a profunctor.
--
-- a Box can also be seen as having an input tape and output tape, thus available for turing and finite-state machine metaphorics.
--
-- >>> :t b
-- b :: Managed (Box Text Text)
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

-- | transduction
-- [wiki](https://en.wikipedia.org/wiki/Transducer) says: "A transducer is a device that converts energy from one form to another." Translated to context, this Transducer converts a stream of type a to a stream of a different type.
--
-- >>> let t = Transducer $ \s -> s & S.takeWhile (/="q") & S.map ("echo: " <>)
-- >>> :t t
-- t :: (Eq b, IsString b, Semigroup b) => Transducer s b b
--
newtype Transducer s a b = Transducer (forall m . Monad m => Stream (Of a) (StateT s m) () -> Stream (Of b) (StateT s m) ())

instance Category (Transducer s) where
    (Transducer t1) . (Transducer t2) = Transducer (t1 . t2)
    id = Transducer id

-- | exposed monad version
newtype TransducerM m s a b = TransducerM (Stream (Of a) (StateT s m) () -> Stream (Of b) (StateT s m) ())

-- | stateless, exposed monad version
newtype TransducerM' m a b = Transducer' (Stream (Of a) m () -> Stream (Of b) m ())

-- | emit - transduce - commit
-- >>> etc () t b
-- echo: hi
-- echo: bye
--
-- with etc, you're in the box, and inside the box, there are no effects: just a stream of 'a's, pure functions and state tracking. It's a nice way to code, and very friendly for the compiler. When the committing and emitting is done, the box collapses to state.
--
-- The combination of an input tape, an output tape, and a state-based stream computation lends itself to the etc computation as a [finite-state transducer](https://en.wikipedia.org/wiki/Finite-state_transducer) or mealy machine.
--
etc ::
  s ->
  Transducer s a b ->
  Managed (Box b a) ->
  IO s
etc st (Transducer stream) ea =
  with ea $ \(Box co em) ->
    ( em |>
      toStream |>
      stream |>
      fromStream)
      co |>
    flip execStateT st

-- | turn an emitter into a stream
toStream :: (MonadBase IO m) => Emitter a -> Stream (Of a) m ()
toStream e = S.untilRight getNext
  where
    getNext = maybe (Right ()) Left <$> liftBase (emit' e)

-- | turn a stream into a committer
fromStream :: (MonadBase IO m) => Stream (Of b) m () -> Committer b -> m ()
fromStream s c = go s
  where
    go str = do
      eNxt <- S.next str -- uncons requires r ~ ()
      forM_ eNxt $ \(a, str') -> do
        continue <- liftBase (commit' c a)
        when continue (go str')

-- | create an emitter from a stream
toEmit :: Buffer a -> Stream (Of a) IO () -> Managed (Emitter a)
toEmit b s = Emitter <$> managed
  (withBuffer b (Committer .> fromStream s) .> fmap snd)

-- | create a committer from a stream consumer
toCommit :: Buffer a -> (Stream (Of a) IO () -> IO r) -> Managed (Committer a)
toCommit b f = Committer <$> managed
  (\c -> withBuffer b c (Emitter .> toStream .> f) |> fmap fst)

-- | create a committer from a fold
toCommitFold :: Buffer a -> L.FoldM IO a () -> Managed (Committer a)
toCommitFold b f = toCommit b (L.impurely S.foldM f .> fmap S.snd')

-- | insert a buffer into a stream
-- todo: generalize
buffStream :: Buffer a -> Stream (Of a) IO () -> (Stream (Of a) IO () -> IO r) -> IO r
buffStream b i o =
  fmap snd $ withBuffer b
  (Committer .> fromStream i)
  (Emitter .> toStream .> o)

-- | connect a committer and emitter action via a buffer
buff :: Buffer a -> (Committer a -> IO b) -> (Emitter a -> IO c) -> IO c
buff b cio eio =
  fmap snd $ withBuffer b (cio <. Committer) (eio <. Emitter)

-- | wait for the first action, and then cancel the second
waitCancel :: IO b -> IO a -> IO b
waitCancel a b =
  withAsync a $ \a' ->
  withAsync b $ \b' -> do
  a'' <- wait a'
  cancel b'
  pure a''

-- | connect a committer and emitter action via a buffer, cancelling the emitter on commision completion.
buff' :: Buffer a -> (Committer a -> IO b) -> (Emitter a -> IO c) -> IO c
buff' b cio eio = withSpawn b
  (\(i,o) ->
     waitCancel
     (eio $ Emitter o)
     (cio $ Committer i)
  )

-- | create a managed committer by fusing a committer
fuseEmit :: Buffer a -> Emitter a -> Managed (Emitter a)
fuseEmit b e = managed $ \e' ->
    withBuffer b (fuse e . Committer) (e' . Emitter) |> fmap snd

-- | hook a committer to a buffer, creating a managed emitter
buffEmit :: Buffer a -> (Committer a -> IO r) -> Managed (Emitter a)
buffEmit b c = managed $ \e ->
  withBuffer b (c . Committer) (e . Emitter) |> fmap snd

-- | create a managed committer by fusing a committer
fuseCommit :: Buffer a -> Committer a -> Managed (Committer a)
fuseCommit b c = managed $ \c' ->
    withBuffer b (c' . Committer) ((`fuse` c) . Emitter) |> fmap fst

-- | hook an emitter to a buffer, creating a managed committer
buffCommit :: Buffer a -> (Emitter a -> IO ()) -> Managed (Committer a)
buffCommit b e = managed $ \c ->
  withBuffer b (c . Committer) (e . Emitter) |> fmap fst

-- | create a managed, double-buffered box hooking up emitter and committer actions
toBox :: Buffer a -> Buffer b -> (Emitter a -> IO ()) -> (Committer b -> IO ()) -> Managed (Box a b)
toBox ba bb eio cio = Box <$> buffCommit ba eio <*> buffEmit bb cio

-- | create a managed box from a box action, forgetting the interactions between emitter and committer that typically gum up the works
toBoxForget :: Buffer a -> Buffer b -> (Box b a -> IO ()) -> Managed (Box a b)
toBoxForget ba bb bio =
  let eio = bio . Box mempty
      cio = bio . (`Box` mempty) in
  toBox ba bb eio cio

-- | a single stdin committer action
cStdin_ :: Committer Text -> IO ()
cStdin_ c = getLine >>= commit c

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
eStdout_ = emit >=> putStrLn

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
showStdout = contramap show <$> (cStdout 1000 unbounded :: Managed (Committer Text))

-- | console box
-- > etc () (Trans $ \s -> s & S.takeWhile (/="q") & S.map ("echo: " <>)) (console 5)
console :: Int -> Buffer Text -> Managed (Box Text Text)
console n b = toBox b b (eStdout n) (cStdin n)

-- | emit lines from a file
emitLines :: FilePath -> Buffer Text -> Managed (Emitter Text)
emitLines filePath b =
  managed (withFile filePath ReadMode) >>= fromHandle .> toEmit b
  where
    fromHandle :: Handle -> Stream (Of Text) IO ()
    fromHandle h = forever $ do
      a <- liftIO $ Text.hGetLine h
      S.yield a

-- | commit lines to a file
commitLines :: FilePath -> Buffer Text -> Managed (Committer Text)
commitLines filePath b =
  managed (withFile filePath WriteMode) >>= toHandle .> toCommit b
  where
    toHandle h = loop where
      loop str = case str of
        S.Return r -> return r
        S.Effect m -> m >>= loop
        S.Step (s :> rest) -> do
          liftIO (Text.hPutStrLn h s)
          loop rest

-- | commit to a monoidal IORef
ioRef :: Semigroup m => IORef m -> Buffer m -> Managed (Committer m)
ioRef ref b = toCommitFold b $
  L.FoldM (\x a -> modifyIORef x (a <>) >> pure x) (pure ref) (const $ pure ())

-- | commit to a list IORef
ioRef' :: IORef [b] -> Buffer b -> Managed (Committer b)
ioRef' ref b = toCommitFold b $
  L.FoldM (\x a -> modifyIORef x (a:) >> pure x) (pure ref) (const $ pure ())

-- | fold an emitter through a transduction, to a list and state
foldToList :: Buffer [b] -> Managed (Emitter a) -> s -> Transducer s a b -> IO ([b], s)
foldToList b e s t = do
    ref <- newIORef []
    r <- etc s t (Box <$> contramap (:[]) <$> ioRef ref b <*> e)
    (,) <$> readIORef ref <*> pure r

-- | fold an emitter thru a transduction, to a list, ignoring the end state
foldToList_ :: Buffer [b] -> Managed (Emitter a) -> s -> Transducer s a b -> IO [b]
foldToList_ b e s t = fmap fst $ foldToList b e s t

-- | sleep for x seconds
sleep :: Double -> IO ()
sleep x = threadDelay (floor $ x * 1e6)

-- | delay by x secs
delay :: Double -> (a -> IO ()) -> (a -> IO ())
delay x io a = sleep x >> io a

-- | send a delayed singleton emission
delayedBy :: Double -> a -> Managed (Emitter a)
delayedBy x q = do
  liftIO $ sleep x
  S.yield q |> toEmit (bounded 1)

-- | keeping a box open sometimes needs a long running emitter
keepOpen :: Managed (Emitter a)
keepOpen = toEmit (bounded 1) $ lift $ threadDelay (365 * 24 * 60 * 60 * 10 ^ 6)

-- | fuse an emitter directly to a committer
fuse :: Emitter a -> Committer a -> IO ()
fuse (Emitter e) (Committer c) = forever $ atomically $ do
    a <- recv e
    maybe (pure ()) (void <$> send c) a

-- | connect an emitter with a committer via an IO computation
fuseM :: (a -> IO b) -> Emitter a -> Committer b -> IO ()
fuseM io (Emitter e) (Committer c) = forever $ do
    a <- atomically $ recv e
    case a of
      Nothing -> pure ()
      Just a' -> do
        b <- io a'
        void $ atomically $ send c b

-- | a modifier that feeds transformed commits back to the emits
feedback :: (b -> a) -> Managed (Box a b) -> Managed (Box a b)
feedback f mbox = managed $ \k ->
  with mbox $ \(Box c e) ->
    do
      fuse (f <$> e) c
      k (Box c e)

-- | it looks like Managed Boxes could form a Category, so here's a wrapper to try it out.
newtype MBox a b = MBox (Managed (Box a b)) deriving (Monoid)

instance Semigroup (MBox a b)

instance Category MBox where
  (MBox a) . (MBox b) = MBox $ do
    (Box c e) <- b
    (Box c' e') <- a
    liftIO $ fuse e c'
    pure (Box c e')

  id = MBox $ do
    (Box c e) <- toBox unbounded unbounded (const mempty) (const mempty)
    liftIO $ fuse e c
    pure (Box c e)

-- | keep valid emissions
keeps
    :: ((b -> Constant (First b) b) -> (a -> Constant (First b) a))
    -- ^
    -> Emitter a
    -- ^
    -> Emitter b
keeps k (Emitter (Input recv_)) = Emitter (Input recv_')
  where
    recv_' = do
        ma <- recv_
        case ma of
            Nothing -> return Nothing
            Just a  -> case match a of
                Nothing -> recv_'
                Just b  -> return (Just b)
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

-- | show an a
cShow :: (Show a) => Managed (Committer Text) -> Managed (Committer a)
cShow c = contramap show <$> c

-- | convert a Pipe to a Transducer
asPipe :: (Monad m) => Pipes.Pipe a b (StateT s m) () -> (Stream (Of a) (StateT s m) () -> Stream (Of b) (StateT s m) ())
asPipe p s = ((s & Pipes.unfoldr S.next) Pipes.>-> p) & S.unfoldr Pipes.next

-- | handle certain commissions
handles
    :: ((b -> Constant (First b) b) -> (a -> Constant (First b) a))
    -- ^
    -> Committer b
    -- ^
    -> Committer a
handles k (Committer (Output send_)) = Committer $ Output (\a -> case match a of
    Nothing -> return False
    Just b  -> send_ b )
  where
    match = getFirst . getConstant . k (Constant . First . Just)

-- | get all commissions as a list
getCommissions :: Managed (Emitter a) -> s -> Transducer s a b -> IO [b]
getCommissions = foldToList_ P.unbounded

-- | get all emissions
getEmissions :: Int -> Managed (Emitter a) -> IO [a]
getEmissions n e = foldToList_ P.unbounded e () (Transducer $ S.take n)
