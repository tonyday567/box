{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | dejavu testing
--
module Main where

import Control.Category (id)
import Control.Monad.Conc.Class as C
import Control.Concurrent.Classy.STM as C
import Box.Box
import Box.Committer
import Box.Emitter
import Box.Broadcast
import Box.Connectors
import Box.Cont
import Box.IO
import Box.Queue
import Box.Stream
import Box.Transducer
import Data.String
import Protolude hiding (STM)
import Test.DejaFu
import Test.DejaFu.Types
import qualified Streaming.Prelude as S
import System.Random
import Control.Lens hiding ((:>), (.>), (<|), (|>))
import Control.Concurrent.Classy.Async as C
import qualified Control.Monad.Trans.State as Trans
import Data.Generics.Labels ()
import Data.Generics.Product

-- | the test box is a pure list emitter into an IORef appending list
testBox :: (MonadConc m) => Int -> m (Cont m (Box (STM m) Int Int), m [Int])
testBox n = do
  (_, c, res) <- cCRef
  let e = toEmit (S.take n $ S.each [0..])
  pure (Box <$> c <*> e, res)

-- | fuse
exFuse :: (MonadConc m) => Int -> m [Int]
exFuse n = do
  (b, res) <- testBox n
  fuse (pure . pure) (liftB <$> b)
  res

exFuseSTM :: (MonadConc m) => Int -> m [Int]
exFuseSTM n = do
  (b, res) <- testBox n
  fuseSTM (pure . pure) b
  res

exEtc :: (MonadConc m) => Int -> m [Int]
exEtc n = do
  (b, res) <- testBox n
  etc () (Transducer id) b
  res

-- | one emitter and 2 committers - STM fusion
e1c2 :: (MonadConc m) => Cont m (Emitter (STM m) b) -> m ([b],[b])
e1c2 e = do
  (_,c1,r1) <- cCRef
  (_,c2,r2) <- cCRef
  fuseSTM (pure . pure) (Box <$> c1 <*> e)
  fuseSTM (pure . pure) (Box <$> c2 <*> e)
  (,) <$> r1 <*> r2

exe1c2 :: (MonadConc m) => Int -> m ([Int],[Int])
exe1c2 n = e1c2 (toEmit (S.take n $ S.each [0..]))

-- | one emitter and 2 committers - IO fusion
e1c2IO :: (MonadConc m) => Cont m (Emitter (STM m) b) -> m ([b],[b])
e1c2IO e = do
  (_,c1,r1) <- cCRef
  (_,c2,r2) <- cCRef
  fuse (pure . pure) (liftB <$> (Box <$> c1 <*> e))
  fuse (pure . pure) (liftB <$> (Box <$> c2 <*> e))
  (,) <$> r1 <*> r2

exe1c2IO :: (MonadConc m) => Int -> m ([Int],[Int])
exe1c2IO n = e1c2IO (toEmit (S.take n $ S.each [0..]))

-- | test when the deterministic takes too long (which is almost always)
t :: (MonadIO n, Eq b, Show b, MonadDejaFu n) =>
     String -> ConcT n b -> n Bool
t l c = dejafuWay (randomly (mkStdGen 42) 1000) defaultMemType l alwaysSame c

main :: IO ()
main = do
  let n = 2
  sequence_ $ autocheck <$> [exFuse n, exFuseSTM n, exEtc n]
  void $ t "e1c2" (exe1c2 10)
  void $ t "e1c2 IO" (exe1c2IO 10)

exc :: (MonadConc m) => Int -> m ([Int],[Int])
exc n = do
  ref <- newIORef 0
  let e = Emitter $ do
        a <- readIORef ref
        if a < n
          then do
            writeIORef ref (a+1)
            pure (Just a)
          else pure Nothing
  (_,c1,r1) <- cCRef
  (_,c2,r2) <- cCRef
  fuse (pure . pure) (Box <$> (liftC <$> c1) <*> pure e)
  fuse (pure . pure) (Box <$> (liftC <$> c2) <*> pure e)
  (,) <$> r1 <*> r2

eCounter :: (C.MonadConc m) => Int -> Int -> m (Emitter m Int, IORef m Int)
eCounter start n = do
  ref <- newIORef start
  pure (
    Emitter $ do
        a <- readIORef ref
        if a < n
          then do
          writeIORef ref (a+1)
          pure (Just a)
        else pure Nothing, ref)

eCounter' :: (C.MonadConc m) => Int -> Int -> m (Cont m (Emitter m Int), IORef m Int)
eCounter' start n = do
  ref <- newIORef start
  pure ( fuseEmitM $ 
    Emitter $ do
        a <- readIORef ref
        if a < n
          then do
          writeIORef ref (a+1)
          pure (Just a)
        else pure Nothing, ref)

exc' :: (MonadIO m, MonadConc m) => Int -> m ([Int],[Int])
exc' n = do
  (b, c) <- broadcast'
  (ec, eref) <- eCounter 0 n 
  let e1 = subscribe' b
  let e2 = subscribe' b
  fuse' (pure . pure) (pure $ Box c ec)
  (_,c1,r1) <- cCRef
  (_,c2,r2) <- cCRef
  fuse (pure . pure) (Box <$> (liftC <$> c1) <*> e1)
  fuse (pure . pure) (Box <$> (liftC <$> c2) <*> e2)
  eres <- readIORef eref
  putStrLn $ "eref: " <> (show eres :: Text)
  (,) <$> r1 <*> r2

-- | a broadcaster 
newtype Broadcaster' m a = Broadcaster'
  { unBroadcast :: TVar (STM m) (Committer m a)
  }

-- | create a (broadcaster, committer)
broadcast' :: (Show a, MonadConc m, MonadIO m) => m (Broadcaster' m a, Committer m a)
broadcast' = do
  ref <- C.atomically $ newTVar mempty
  let com = Committer $ \a -> do
        putStrLn $ "broadcaster': " <> (show a :: Text)
        c <- C.atomically $ readTVar ref
        commit c a
  pure (Broadcaster' ref, com)

-- | subscribe to a broadcaster
subscribe' :: (Show a, MonadIO m, MonadConc m) => Broadcaster' m a -> Cont m (Emitter m a)
subscribe' (Broadcaster' tvar) = Cont $ \e -> queueELog cio e
  where
    cio c = C.atomically $ modifyTVar' tvar (mappend c)

-- * primitives
-- | fuse an emitter directly to a committer
fuse_' :: (Show a, MonadIO m) => Emitter m a -> Committer m a -> m ()
fuse_' e c = go
  where
    go = do
      a <- emit e
      putStrLn $ "fuse_' emit: " <> (show a :: Text)
      c' <- maybe (pure False) (commit c) a
      putStrLn $ "fuse_' commit: " <> (show c' :: Text)
      when c' go

fuse' :: (Show b, MonadIO m) => (a -> m (Maybe b)) -> Cont m (Box m b a) -> m ()
fuse' f box = with box $ \(Box c e) -> fuse_' (emap f e) c

-- exEmerge :: (MonadIO m, MonadConc m) => Int -> Int -> Int -> Int -> m [Int]
exEmerge :: MonadConc m => Int -> Int -> Int -> Int -> m ([Int], Int, Int)
exEmerge st1 st2 n1 n2 = do
  (e1, eref1) <- eCounter st1 n1
  (e2, eref2) <- eCounter st2 n2
  (_,c1,r1) <- cCRef
  fuse (pure . pure) $ Box <$> (liftC <$> c1) <*> emergeM (pure (e1, e2))
  (,,) <$> r1 <*> readIORef eref1 <*> readIORef eref2

exEmergeM :: MonadConc m => Int -> Int -> Int -> Int -> m ([Int], Int, Int)
exEmergeM st1 st2 n1 n2 = do
  (e1, eref1) <- eCounter' st1 n1
  (e2, eref2) <- eCounter' st2 n2
  (_,c1,r1) <- cCRef
  fuse (pure . pure) $ Box <$> (liftC <$> c1) <*> emergeM ((,) <$> e1 <*> e2)
  (,,) <$> r1 <*> readIORef eref1 <*> readIORef eref2


temerge :: IO Bool
temerge = dejafuWay
    (randomly (mkStdGen 42) 1000)
    defaultMemType
    "test emergeM"
    alwaysSame
    (exEmergeM 0 0 2 2)

exCSplit :: MonadConc m => Int -> Int -> m [Int]
exCSplit st1 n1 = do
  (e1, _) <- eCounter' st1 n1
  (_,c1,r1) <- cCRef
  fuse (pure . pure) $ Box <$> (liftC <$> liftA2 (<>) c1 c1) <*> e1
  r1

contCommit' :: Either (Committer m Int) (Committer m Int) -> Committer m Int
contCommit' ec =
  Committer $ \a ->
    case ec of
      Left lc -> commit (contramap (100+) lc) a
      Right rc -> commit rc a

splitCommitM :: (MonadConc m) =>
     Cont m (Committer m a)
  -> Cont m (Either (Committer m a) (Committer m a))
splitCommitM c =
  Cont $ \kk ->
    with c $ \c' ->
      fst <$>
      C.concurrently
        (queueCM (kk . Left) (`fuse_` c'))
        (queueCM (kk . Right) (`fuse_` c'))


counter :: (MonadState Int m) => Int -> StateT Int m (Emitter m Int)
counter n =
  pure $
    Emitter $ do
        a <- Protolude.get
        case a < n of
          False -> pure Nothing
          True -> do
            put $ a + 1
            pure (Just a)

counterT :: (Num a, Ord a, Monad m) => a -> Emitter (StateT a m) a
counterT n =
    Emitter $ do
        a <- Trans.get
        case a < n of
          False -> pure Nothing
          True -> do
            Trans.put $ a + 1
            pure (Just a)


rememberer :: (MonadState [Int] m) => StateT [Int] m (Committer m Int)
rememberer =
  pure $
    Committer $ \a -> do
        modify (a:)
        pure True

remembererT :: (Monad m) => Committer (StateT [a] m) a
remembererT =
    Committer $ \a -> do
        Trans.modify (a:)
        pure True


data StateExs = StateExs { count :: Int, result :: [Int]} deriving (Show, Eq, Generic)

boxCount ::
  (MonadConc m, MonadState StateExs m) =>
  Int ->
  m (Box m Int Int)
boxCount n = Box <$> pure rememberer' <*> pure counter' where
  counter' =
    Emitter $ do
        a <- use #count
        case a < n of
          False -> pure Nothing
          True -> do
            #count += 1
            pure (Just a)
  rememberer' =
    Committer $ \a -> do
        #result %= (a:)
        pure True

countEmitter :: (Ord a, Num a, MonadState s m, Data.Generics.Product.HasField "count" s s a a) => a -> Emitter m a
countEmitter n = Emitter $ do
  a <- use (field @"count")
  case a < n of
    False -> pure Nothing
    True -> do
      field @"count" += 1
      pure (Just a)

resultCommitter :: (MonadState s m, Data.Generics.Product.HasField "result" s s [a] [a]) => Committer m a
resultCommitter = Committer $ \a -> do
  field @"result" %= (a:)
  pure True

-- boxCount' :: (MonadState StateExs m, MonadConc m) => Int -> Box m Int Int
-- boxCount' n = Box (zoom #result resultCommitter) (zoom #count (countEmitter n))

exs :: (MonadConc m) => Int -> m [Int]
exs n = do
  (StateExs _ res) <- execStateT
    (fuse (pure . pure) (pure $ Box resultCommitter (countEmitter n)))
    (StateExs 0 [])
  pure (reverse res)

fuse'' :: (Show b, MonadIO m) => (a -> m (Maybe b)) -> Cont m (Box m b a) -> m ()
fuse'' f box = with box $ \(Box c e) -> fuse_' (emap f e) c


-- | emitter hooked into broadcasting is broken ...
-- > exbr 2
-- ([],[])
--
broadcasting :: MonadConc m => Cont m (Emitter (STM m) b) -> m ([b], [b])
broadcasting e = do
  (b, c) <- C.atomically broadcast
  let e1 = subscribe b
  let e2 = subscribe b
  fuseSTM (pure . pure) (Box <$> pure c <*> e)
  (_,c1,r1) <- cCRef
  (_,c2,r2) <- cCRef
  fuseSTM (pure . pure) (Box <$> c1 <*> e1)
  fuseSTM (pure . pure) (Box <$> c2 <*> e2)
  (,) <$> r1 <*> r2

exbrPure :: (MonadConc m) => Int -> m ([Int],[Int])
exbrPure n = broadcasting (toEmit (S.take n $ S.each [0..]))

-- > exbrIO 2
-- 1
-- 2
-- ([],[])
--
exbrIO :: Int -> IO ([Text],[Text])
exbrIO n = broadcasting (eStdin n)


