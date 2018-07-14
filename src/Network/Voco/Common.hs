{-# LANGUAGE OverloadedStrings #-}

-- | Module defining common or even ubiquitous bots.
module Network.Voco.Common
    ( pingpong
    , delayedJoin
    , delayedJoinBatch
    , standard
    , logRaw
    ) where

import Control.Lens
import Control.Monad.Chan
import Control.Monad.Logger
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Network.Voco.Action
import Network.Voco.Bot
import Network.Voco.Combinators
import Network.Yak
import Network.Yak.Client
import Network.Yak.Responses

import qualified Data.List.NonEmpty as N
import qualified Data.Text.Encoding as T

-- | A bot responding to ping.
pingpong :: MonadChan m => Bot m Ping ()
pingpong = do
    ping <- query
    pong (ping ^. pingServer1) (ping ^. pingServer2)

-- | A bot executing joins to a given list of channels after the welcome
-- message. All channels are sent in one go! See 'delayedJoinBatch' for a
-- batched variant.
delayedJoin :: MonadChan m => NonEmpty Channel -> Bot m RplWelcome ()
delayedJoin = join

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
    let (front, back) = splitAt n xs
    in front : chunksOf n back

-- | Like 'delayedJoin' but sends join messages in batches of @n@.
delayedJoinBatch :: MonadChan m => Int -> NonEmpty Channel -> Bot m RplWelcome ()
delayedJoinBatch n cs =
    let chunked = chunksOf n . N.toList $ cs
     in mapM_ (join . N.fromList) chunked

-- | A bot logging all raw IRC data. Note that this does /only/ log incoming
-- data, since a bot cannot listen to other bots (in the context of a composed
-- bot). The log messages will be written at a custom log level, prefixed with
-- @RAW@.
logRaw :: MonadLogger m => Bot m ByteString ()
logRaw = do
    i <- query
    logOtherN (LevelOther "RAW") (T.decodeUtf8 i)

-- | A standard IRC bot handlings pings and initial joins.
standard :: MonadChan m => NonEmpty Channel -> Bot m ByteString ()
standard cs = irc pingpong <|> irc (delayedJoinBatch 8 cs)
