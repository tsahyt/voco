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

import qualified Data.Text as T

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
chan = "#voco-example"

main :: IO ()
main = 
    botloop
        server
        (NT $ runStderrLoggingT)
        (standard [chan] <> logRaw <> irc interaction <> irc ongoing <> irc namesBot <> irc userhostBot)

ongoing :: MonadIO m => AutoBot m ()
ongoing = every (minutes 1) $ message chan "boop"

interaction :: (MonadChan m, MonadIO m) => Bot m Privmsg ()
interaction =
    on (view $ privmsgMessage) . filterB (== "!!start") . request' $ do
        message chan "will echo back the next statement"
        msg <- view (privmsgMessage) <$> recv
        message chan $ "you said: " <> msg

namesBot :: MonadIO m => Bot m Privmsg ()
namesBot =
    on (view $ privmsgMessage) . filterB (== "!!names") . asyncV $ do
        nms <- request $ names chan
        message chan . Message . T.intercalate " " $ nms

userhostBot :: (MonadChan m, MonadIO m) => Bot m Privmsg ()
userhostBot =
    on (view $ privmsgMessage . _Wrapped) . filterB ("!!host " `T.isPrefixOf`) $ do
        i <- query
        uh <- request . userhost $ T.drop 7 i
        message chan . Message . T.pack . show $ uh
