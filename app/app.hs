-- |

module Main where

import Prelude
import Data.Bool
import Data.Foldable
import Box

data Teletype m a where
  ReadTTY  :: Teletype m String
  WriteTTY :: String -> Teletype m ()

echo :: (Monad m, Eq a, Monoid a) => Box m a a -> m ()
echo = fuse (\x -> bool (pure (Just x)) (pure Nothing) (x == mempty))

