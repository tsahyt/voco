{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Main where

import Control.Lens ((^.))
import Control.Monad
import Data.ByteString (ByteString)
import Control.Category
import Network.Voco
import Network.Yak.Types
import Network.Yak.Client

import Prelude hiding ((.), id)

server :: IRCServer
server = IRCServer
    { serverHost = "irc.snoonet.org"
    , serverPort = PortNumber 6667
    , serverPass = Nothing
    , botUser = "botnetscousin"
    , botRealname = "bot"
    , botNickname = "botnetscousin"
    }

main :: IO ()
main = botloop server id bot

bot :: Monad m => Bot m ByteString ()
bot =
    parsed (delayedJoin [Channel "#linuxmasterrace"]) <|> parsed pingpong <|>
    parsed response

response :: Monad m => Bot m Privmsg ()
response = do
    i <- query
    guard (i ^. privmsgMessage == "!!foo")
    message (Channel "#linuxmasterrace") "foo"
