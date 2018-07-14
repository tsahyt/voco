{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Category
import Control.Lens
import Control.Concurrent
import Control.Monad
import Control.Monad.Logger
import Control.Monad.State
import Control.Natural
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Text (Text)
import Network.Voco
import Network.Yak.Client
import Network.Yak.Responses
import Network.Yak.Types

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import Prelude hiding ((.), id)

server :: IRCServer
server =
    IRCServer
    { serverConnectionParams =
        ConnectionParams
            { connectionHostname = "irc.snoonet.org"
            , connectionPort = 6697
            , connectionUseSecure = Just $ TLSSettingsSimple False False False
            , connectionUseSocks = Nothing }
    , serverPass = Nothing
    , botUser = "voco-example"
    , botRealname = "bot"
    , botNickname = "voco-example"
    }

chan :: Channel
chan = Channel "#voco-example"

allowedNick :: Nickname
allowedNick = "tsahyt"

main :: IO ()
main = 
    botloop
        server
        (NT $ runStderrLoggingT)
        (standard [chan] <> logRaw)

ongoing :: MonadIO m => AutoBot m ()
ongoing = every (seconds 3) $ message chan "boop"
