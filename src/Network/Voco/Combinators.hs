module Network.Voco.Combinators
    ( parsed
    , on
    , abort
    , refine
    , query
    , natural
    -- * Re-exports for convenience
    , module Control.Applicative
    , module Data.Profunctor
    ) where

import Control.Applicative hiding (WrappedArrow(..))
import Control.Monad.Random.Class
import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.Profunctor
import Network.Voco.Bot
import Network.Yak

-- | Transform a bot on some fetchable IRC message (or coproduct thereof) into a
-- bot on 'ByteString', i.e. on raw IRC input.
parsed :: (Applicative m, Fetch i) => Bot m i o -> Bot m ByteString o
parsed = refine fetch

-- | Synonym for 'lmap'.
on :: Functor m => (i' -> i) -> Bot m i o -> Bot m i' o
on = lmap

-- | Guard a bot with a random chance of triggering. The 'Double' argument is
-- expected to describe a probability, i.e. it must be a number between 0 and 1.
-- Numbers less than 0 will result in never triggering, whereas numbers larger
-- than 1 will always trigger.
chance :: MonadRandom m => Double -> Bot m i o -> Bot m i o
chance p b = do
    x <- getRandomR (0, 1)
    guard (x <= p)
    b

-- | Filter a bot based on a predicate on the input.
filterB :: Monad m => (i -> Bool) -> Bot m i o -> Bot m i o
filterB p b = do
    i <- query
    guard (p i)
    b

-- | Special 'filterB' using 'Host' predicates. This is useful to ignore certain
-- users for example.
filterH :: Monad m => (Host -> Bool) -> Bot m (Msg c p) o -> Bot m (Msg c p) o
filterH p b = do
    i <- query
    case msgPrefix i of
        Just (PrefixUser h) -> guard (p h) *> b
        _ -> empty
