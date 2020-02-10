{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

-- | based on https://github.com/Gabriel439/Haskell-MVC-Updates-Library
module Box.Updater
  ( Updater (..),
    updater,
    listen,
    updates,
  )
where

import Box
import Control.Applicative (Applicative ((<*>), pure))
import Control.Foldl (Fold (..), FoldM (..))
import qualified Control.Foldl as Foldl
import Control.Monad.Conc.Class as C
import qualified GHC.Conc

-- | An updater of a value a, where the updating process consists of an IO fold over an emitter
data Updater a
  = forall e.
    Updater
      (FoldM IO e a)
      (Cont IO (Emitter GHC.Conc.STM e))

instance Functor Updater where
  fmap f (Updater fold' e) = Updater (fmap f fold') e

{-
> onLeft (f <*> x) = onLeft f <*> onLeft x
>
> onLeft (pure r) = pure r
-}
onLeft :: Monad m => FoldM m a b -> FoldM m (Either a x) b
onLeft (FoldM step begin done) = FoldM step' begin done
  where
    step' x (Left a) = step x a
    step' x _ = return x

{-
> onRight (f <*> x) = onRight f <*> onRight x
>
> onRight (pure r) = pure r
-}
onRight :: Monad m => FoldM m a b -> FoldM m (Either x a) b
onRight (FoldM step begin done) = FoldM step' begin done
  where
    step' x (Right a) = step x a
    step' x _ = return x

instance Applicative Updater where

  pure a = Updater (pure a) mempty

  (Updater foldL eL) <*> (Updater foldR eR) = Updater foldT eT
    where
      foldT = onLeft foldL <*> onRight foldR
      eT = fmap (fmap Left) eL <> fmap (fmap Right) eR

-- | Create an 'Updater' value using a pure 'Fold'
updater :: Fold e a -> Cont IO (Emitter GHC.Conc.STM e) -> Updater a
updater fold' = Updater (Foldl.generalize fold')

-- | run an action on each update
-- > listen mempty = id
-- >
-- > listen (f <> g) = listen g . listen f
listen :: (a -> IO ()) -> Updater a -> Updater a
listen handler (Updater (FoldM step begin done) mController) =
  Updater (FoldM step' begin' done) mController
  where
    begin' = do
      x <- begin
      b <- done x
      handler b
      return x
    step' x a = do
      x' <- step x a
      b <- done x'
      handler b
      return x'

-- | Convert an 'Updater' to an Emitter continuation.
updates :: Updater a -> Cont IO (Emitter GHC.Conc.STM a)
updates (Updater (FoldM step begin done) e) = Cont $ \e' -> queueE' cio e'
  where
    ioref c = do
      x <- begin
      a <- done x
      _ <- atomically $ commit c a
      C.newIORef x
    cio c =
      with e $ \e' -> do
        ioref' <- ioref c
        x <- C.readIORef ioref'
        e'' <- atomically $ emit e'
        case e'' of
          Nothing -> pure ()
          Just e''' -> do
            x' <- step x e'''
            a <- done x'
            _ <- atomically $ commit c a
            C.writeIORef ioref' x'
