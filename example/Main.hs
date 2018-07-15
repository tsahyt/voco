{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Lens
import Control.Monad.Logger
import Control.Monad.State
import Control.Natural
import Data.Monoid
import Network.Voco
import Network.Yak.Types
import Network.Yak.Client

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
        (standard [chan] <> logRaw <> irc interaction)

ongoing :: MonadIO m => AutoBot m ()
ongoing = every (seconds 3) $ message chan "boop"

interaction :: (MonadChan m, MonadIO m) => Bot m Privmsg ()
interaction =
    on (view $ privmsgMessage) $
        filterB (== "!!start") $ asyncV . request $ do
            message chan "will echo back the next statement"
            msg <- view (privmsgMessage) <$> recv
            message chan $ "you said: " <> msg
