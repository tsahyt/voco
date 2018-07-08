{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Category
import Control.Lens (view)
import Control.Monad
import Data.Text (Text)
import Data.Monoid
import Data.ByteString (ByteString)
import Network.Voco
import Network.Yak.Client
import Network.Yak.Types

import Prelude hiding ((.), id)

server :: IRCServer
server =
    IRCServer
    { serverHost = "irc.snoonet.org"
    , serverPort = PortNumber 6667
    , serverPass = Nothing
    , botUser = "botnetscousin"
    , botRealname = "bot"
    , botNickname = "botnetscousin"
    }

chan :: Channel
chan = Channel "#zowlyfon"

main :: IO ()
main = botloop server id (standard [chan] <> irc bot)

bot :: Monad m => Bot m Privmsg ()
bot = view privmsgMessage `on` asum @[]
    [ filterB (== "!!foo") $ message chan "foo"
    , filterB (== "!!bar") $ message chan "bar!"
    , filterB (== "!!quux") $ message chan "quux!!"
    ]
