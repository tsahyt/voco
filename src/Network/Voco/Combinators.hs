module Network.Voco.Combinators
    ( irc
    , parsed
    , on
    , abort
    , refine
    , query
    , natural
    , divide
    , async
    -- * Filtering
    , filterB
    , filterH
    , filterM
    , byNick
    , onChannel
    -- * Re-exports for convenience
    , Alternative(..)
    , asum
    , guard
    , Profunctor(..)
    ) where

import Control.Applicative hiding (WrappedArrow(..))
import Control.Lens (toListOf)
import Control.Monad (guard)
import Control.Monad.Random.Class
import Data.Attoparsec.Text (Parser, parseOnly)
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Profunctor
import Data.Text (Text)
import Network.Voco.Bot
import Network.Yak
import Network.Yak.Client (HasNick(..), HasChannel(..))

-- | Transform a bot on some fetchable IRC message (or coproduct thereof) into a
-- bot on 'ByteString', i.e. on raw IRC input.
irc :: (Applicative m, Fetch i) => Bot m i o -> Bot m ByteString o
irc = refine fetch

-- | Precompose a bot with an "attoparsec" parser.
parsed :: Applicative m => Parser i -> Bot m i o -> Bot m Text o
parsed p = refine (either (const Nothing) Just . parseOnly p)

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

-- | Like 'filterB' but taking a monadic predicate.
filterM :: Monad m => (i -> m Bool) -> Bot m i o -> Bot m i o
filterM p b = do
    i <- query
    x <- liftBot $ p i
    guard x
    b

-- | Filter input by nicknames, convience function using 'filterB'. Messages
-- containing multiple nicknames will need to match the predicate for /all/
-- nicks!
byNick :: (HasNick i, Monad m) => (Nickname -> Bool) -> Bot m i o -> Bot m i o
byNick p = filterB (all p . toListOf nick)

-- | Filter input on 'Channel's, convience function using 'filterB'. Messages
-- containing multiple channels will need to match the predicate for /all/
-- channels!
onChannel :: (HasChannel i, Monad m) => (Channel -> Bool) -> Bot m i o -> Bot m i o
onChannel p = filterB (all p . toListOf channel)
