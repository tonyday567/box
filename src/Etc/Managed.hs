{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Etc.Managed
  ( -- * Managed
    Managed
  , managed
  , managed_
  , with
  , runManaged
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Applicative
import Data.Monoid (Monoid(..))

import Data.Semigroup (Semigroup(..))

-- | A managed resource that you acquire using `with`
newtype Managed m a = Managed { (>>-) :: forall r . (a -> m r) -> m r }

instance Functor (Managed m) where
    fmap f mx = Managed (\return_ ->
        mx >>- \x ->
        return_ (f x) )

instance Applicative (Managed m) where
    pure r    = Managed (\return_ ->
        return_ r )

    mf <*> mx = Managed (\return_ ->
        mf >>- \f ->
        mx >>- \x ->
        return_ (f x) )

instance Monad (Managed m) where
    return r = Managed (\return_ ->
        return_ r )

    ma >>= f = Managed (\return_ ->
        ma  >>- \a ->
        f a >>- \b ->
        return_ b )

instance (MonadIO m) => MonadIO (Managed m) where
    liftIO m = Managed (\return_ -> do
        a <- liftIO m
        return_ a )

instance (Semigroup a) => Semigroup (Managed m a) where
    (<>) = liftA2 (<>)

instance (Functor m, Semigroup a, Monoid a) => Monoid (Managed m a) where
    mempty = pure mempty
    mappend = (<>)

-- | Build a `Managed` value
managed :: (forall r . (a -> m r) -> m r) -> Managed m a
managed = Managed

-- | Like 'managed' but for resource-less operations.
managed_ :: (forall r. m r -> m r) -> Managed m ()
managed_ f = managed $ \g -> f $ g ()

{-| Acquire a `Managed` value

    This is a potentially unsafe function since it allows a resource to escape
    its scope.  For example, you might use `Managed` to safely acquire a
    file handle, like this:

> import qualified System.IO as IO
>
> example :: Managed Handle
> example = managed (IO.withFile "foo.txt" IO.ReadMode)

    ... and if you never used the `with` function then you would never run the
    risk of accessing the `Handle` after the file was closed.  However, if you
    use `with` then you can incorrectly access the handle after the handle is
    closed, like this:

> bad :: IO ()
> bad = do
>     handle <- with example return
>     IO.hPutStrLn handle "bar"  -- This will fail because the handle is closed

    ... so only use `with` if you know what you are doing and you're returning
    a value that is not a resource being managed.
-}
with :: Managed m a -> (a -> m r) -> m r
with = (>>-)

-- | Run a `Managed` computation, enforcing that no acquired resources leak
runManaged :: (Monad m) => Managed m () -> m ()
runManaged m = m >>- return

{- $reexports
    "Control.Monad.IO.Class" re-exports 'MonadIO'
-}
