-- | Module defining common or even ubiquitous bots.
module Network.Voco.Common
    ( pingpong
    , delayedJoin
    , delayedJoinBatch
    , standard
    ) where

import Control.Lens
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Network.Voco.Action
import Network.Voco.Bot
import Network.Voco.Combinators
import Network.Yak
import Network.Yak.Client
import Network.Yak.Responses

-- | A bot responding to ping.
pingpong :: Monad m => Bot m Ping ()
pingpong = do
    ping <- query
    pong (ping ^. pingServer1) (ping ^. pingServer2)

-- | A bot executing joins to a given list of channels after the welcome
-- message. All channels are sent in one go! See 'delayedJoinBatch' for a
-- batched variant.
delayedJoin :: Monad m => NonEmpty Channel -> Bot m RplWelcome ()
delayedJoin = join

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
    let (front, back) = splitAt n xs
    in front : chunksOf n back

-- | Like 'delayedJoin' but sends join messages in batches of @n@.
delayedJoinBatch :: Monad m => Int -> NonEmpty Channel -> Bot m RplWelcome ()
delayedJoinBatch n cs =
    let chunked = chunksOf n . N.toList $ cs
     in mapM_ (join . N.fromList) chunked

-- | A standard IRC bot handlings pings and initial joins.
standard :: Monad m => NonEmpty Channel -> Bot m ByteString ()
standard cs = irc pingpong <|> irc (delayedJoinBatch 8 cs)
