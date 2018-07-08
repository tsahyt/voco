{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Category
import Control.Natural
import Control.Lens (view, _Wrapped)
import Control.Monad
import Control.Monad.State
import Data.Text (Text)
import Data.Monoid
import Data.ByteString (ByteString)
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
    , botUser = "botnetscousin"
    , botRealname = "bot"
    , botNickname = "botnetscousin"
    }

chan :: Channel
chan = Channel "#zowlyfon"

main :: IO ()
main = botloop server (NT $ flip evalStateT 0) (standard [chan] <> irc bot)

addParse :: A.Parser (Int, Int)
addParse = (,) <$> (A.string "!!add" *> A.decimal <* A.skipSpace) <*> A.decimal

bot :: MonadState Int m => Bot m Privmsg ()
bot = view (privmsgMessage . _Wrapped) `on` asum @[]
    [ filterB (== "!!foo") $ message chan "foo"
    , parsed addParse $ do
        (a,b) <- query
        message chan (Message . T.pack . show $ a + b)
    , filterB (== "!!count") $ do
        i <- get
        modify succ
        message chan $ Message . T.pack . show $ i
    ]
