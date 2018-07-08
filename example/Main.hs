{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Category
import Control.Lens (_Wrapped, view)
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
import Network.Yak.Types

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import Prelude hiding ((.), id)

server :: IRCServer
server =
    IRCServer
    { serverHost = "irc.snoonet.org"
    , serverPort = PortNumber 6667
    , serverPass = Nothing
    , botUser = "voco-example"
    , botRealname = "bot"
    , botNickname = "voco-example"
    }

chan :: Channel
chan = Channel "#voco-example"

main :: IO ()
main =
    botloop
        server
        (NT $ \m -> runStderrLoggingT (evalStateT m 0))
        (standard [chan] <> logRaw <> irc bot)

addParse :: A.Parser (Int, Int)
addParse =
    (,) <$> (A.string "!!add" *> A.skipSpace *> A.decimal <* A.skipSpace) <*>
    A.decimal

bot :: (MonadIO m, MonadState Int m) => Bot m Privmsg ()
bot =
    view (privmsgMessage . _Wrapped) `on`
    asum @[]
        [ filterB (== "!!foo") $ message chan "foo"
        , parsed addParse $ do
              (a, b) <- query
              message chan $ Message . T.pack . show $ a + b
        , filterB (== "!!count") $ do
              i <- get
              modify succ
              message chan $ Message . T.pack . show $ i
        , filterB (== "!!wait") $ do
              message chan "waiting for a while"
              void . async $ do
                  liftIO (threadDelay 10000000)
                  message chan "done waiting"
        ]
