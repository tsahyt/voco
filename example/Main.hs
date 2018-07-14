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
    { serverHost = "irc.snoonet.org"
    , serverPort = PortNumber 6667
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
        (NT $ \m -> evalStateT m (0,0))
        (standard [chan] <> irc ongoing)

addParse :: A.Parser (Int, Int)
addParse =
    (,) <$> (A.string "!!add" *> A.skipSpace *> A.decimal <* A.skipSpace) <*>
    A.decimal

ongoing :: MonadIO m => LongBot m ()
ongoing = threes <> sixes
  where
    threes = every (seconds 3) $ message chan "beep"
    sixes = every (seconds 6) $ message chan "boop"

{-
 -bot :: Bot (StateT (Int, Int) IO) Privmsg ()
 -bot =
 -    filterH ((== allowedNick) . view hostNick) $ 
 -    view (privmsgMessage . _Wrapped) `on`
 -    (echo <|> add <|> zoom _1 count <|> zoom _2 countdown <|> wait)
 -  where
 -    echo = filterB (== "!!foo") $ message chan "foo"
 -    add =
 -        parsed addParse $ do
 -            (a, b) <- query
 -            message chan $ Message . T.pack . show $ a + b
 -    count =
 -        filterB (== "!!count") $ do
 -            i <- get
 -            modify succ
 -            message chan $ Message . T.pack . show $ i
 -    countdown =
 -        filterB (== "!!countdown") $ do
 -            i <- get
 -            modify pred
 -            message chan $ Message . T.pack . show $ i
 -    wait =
 -        filterB (== "!!wait") $ do
 -            message chan "waiting for a while"
 -            void . async $ do
 -                liftIO (threadDelay 10000000)
 -                message chan "done waiting"
 -}
