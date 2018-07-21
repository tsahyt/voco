{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Voco.Combinators
    ( irc
    , parsed
    , on
    , abort
    , refine
    , query
    , divide
    , natural
    , chance
    -- * Asynchronous Bots
    , async
    , async'
    , asyncV
    , asyncV'
    -- * Requests
    , request
    , request'
    -- * Filtering
    , filterB
    , filterH
    , filterM
    , inQuery
    , onChannel
    -- * Self-Running Bots
    , AutoBot
    , endless
    , endless'
    , every
    -- ** Time Functions
    , TimeSpec
    , seconds
    , minutes
    , hours
    , days
    , weeks
    -- * Re-exports for convenience
    , Alternative(..)
    , asum
    , guard
    , Profunctor(..)
    ) where

import Control.Applicative hiding (WrappedArrow(..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async)
import Control.Lens (toListOf, (^.), to)
import Control.Monad (forever, guard, void)
import Control.Monad.IO.Class
import Control.Monad.Random.Class
import Control.Natural
import Data.Attoparsec.Text (Parser, parseOnly)
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Profunctor
import Data.Text (Text)
import Network.Voco.Core
import Network.Yak
import Network.Yak.Client (HasChannel(..), Privmsg, privmsgTargets)
import Network.Yak.Responses (RplWelcome)

import qualified Data.List.NonEmpty as N

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

-- | Combinator guarding bots responding to Privmsg for those messages to
-- happen in a query, i.e. as a personal message. If the incoming message is a
-- PM, the given bot will be called with the incoming host as an argument.
inQuery :: Monad m => (Host -> Bot m Privmsg o) -> Bot m Privmsg o
inQuery b = do
    i <- query
    case i ^. privmsgTargets . to N.head of
        Left _ -> empty
        Right _ ->
            case msgPrefix i of
                Just (PrefixUser x) -> b x
                _ -> empty

-- | Filter input on 'Channel's, convience function using 'filterB'. Messages
-- containing multiple channels will need to match the predicate for /all/
-- channels!
onChannel ::
       (HasChannel i, Monad m) => (Channel -> Bool) -> Bot m i o -> Bot m i o
onChannel p = filterB (all p . toListOf channel)

-- | Like 'async' but with a natural transformation to easily build up a monad
-- stack for the asynchronous 'Bot'.
async' :: MonadIO m => (n :~> IO) -> Bot n i a -> Bot m i (Async (Maybe a))
async' nt b = async (natural nt b)

-- | Voided version of 'async'
asyncV :: MonadIO m => Bot IO i a -> Bot m i ()
asyncV = void . async

-- | Voided version of 'async' with a natural transformation.
asyncV' :: MonadIO m => (n :~> IO) -> Bot n i a -> Bot m i ()
asyncV' n = void . async' n

-- | > request' = asyncV . request
request' :: MonadIO m => Req a -> Bot m i ()
request' = asyncV . request

-- | A synonym for long running bots. These bots are supposed to run
-- asynchronously, and are built using the 'endless' combinator. They trigger
-- once and only once in any IRC session, after the welcome reply from the IRC
-- server.
type AutoBot m o = Bot m RplWelcome o

-- | Runs a bot endlessly in a loop. This acts like the 'forever' combinator,
-- but lifted to bots.
endless :: MonadIO m => (forall i. Bot IO i ()) -> AutoBot m ()
endless b = void . async $ forever b

-- | Like 'endless' but with a natural transformation to easily build up a
-- monad stack for the endless 'Bot'
endless' :: MonadIO m => (n :~> IO) -> (forall i. Bot n i ()) -> AutoBot m ()
endless' nt b = endless (natural nt b)

-- | Intervals of time. 'fromInteger' takes microseconds as a unit.
newtype TimeSpec =
    TimeSpec Int
    deriving (Eq, Show, Ord, Num)

seconds :: Int -> TimeSpec
seconds = TimeSpec . (* 1000000)

minutes :: Int -> TimeSpec
minutes = seconds . (* 60)

hours :: Int -> TimeSpec
hours = minutes . (* 60)

days :: Int -> TimeSpec
days = hours . (* 24)

weeks :: Int -> TimeSpec
weeks = days . (* 7)

-- | Create a self-running bot which operates once every @k@ seconds, as given
-- by the 'TimeSpec' value.
every :: MonadIO m => TimeSpec -> (forall i. Bot IO i ()) -> AutoBot m ()
every (TimeSpec µs) b = endless $ liftIO (threadDelay µs) *> b
