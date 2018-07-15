{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Network.Voco.IO
    ( IRCServer(..)
    , botloop
    -- * Low-Level
    , bootstrap
    , botloop'
    , connectIRC
    -- * Testing
    , testloop
    -- * Re-Exports
    -- ** Natural Transformations
    , module Control.Natural
    -- ** Networking
    , ConnectionParams(..)
    , TLSSettings(..)
    , ProxySettings(..)
    , HostName
    , PortNumber(..)
    ) where

import Control.Concurrent (forkIO)
import Control.Monad (forever, void)
import Control.Monad.Chan
import Control.Monad.IO.Class
import Control.Natural
import Data.ByteString (ByteString)
import Data.Maybe (maybeToList)
import Data.Monoid
import Data.Text (Text)
import Network (HostName, PortNumber(..))
import Network.Connection
import Network.Voco.Bot
import Network.Voco.Action
import Network.Voco.Request
import Network.Yak
import Network.Yak.Client
import System.IO

import qualified Data.ByteString as B

-- | Generalized bot loop function, taking a way to read a line from the
-- network, a way to send a line to the network, a natural transformation from
-- the underlying monad of 'Bot' to 'IO', as well as a 'Bot' of a suitable type.
botloop' ::
       MonadIO m
    => IO ByteString
    -> (ByteString -> IO ())
    -> (m :~> IO)
    -> Bot m ByteString ()
    -> IO ()
botloop' get send nt bot = do
    chan <- newChan
    forkIO $ out chan
    forkIO $ reqs chan
    nt $$ forever (process chan)
  where
    out chan = do
        x <- readChan chan
        send . emitSome . getAction $ x
        out chan
    reqs chan = pure ()
    process chan = do
        msg <- liftIO $ get
        runBot bot chan msg

-- | Bootstrapping messages to be sent to an IRC server.
bootstrap :: IRCServer -> [ByteString]
bootstrap server =
    map emitSome $
    [SomeMsg (build p :: Pass) | p <- maybeToList (serverPass server)] ++
    [ SomeMsg
          (build (botUser server) 0 Unused (Message $ botRealname server) :: User)
    , SomeMsg (build (botNickname server) :: Nick)
    ]

connectIRC :: IRCServer -> IO Connection
connectIRC server = do
    ctx <- initConnectionContext
    conn <- connectTo ctx $ serverConnectionParams server
    -- run bootstrap sequence
    mapM_ (\x -> connectionPut conn $ x <> "\r\n") (bootstrap server)
    pure conn

botloop :: MonadIO m => IRCServer -> (m :~> IO) -> Bot m ByteString () -> IO ()
botloop server nt bot = do
    conn <- connectIRC server
    let get = connectionGetLine 4096 conn
        send x = connectionPut conn (x <> "\r\n")
    botloop' get send nt bot

testloop :: MonadIO m => (m :~> IO) -> Bot m ByteString () -> IO ()
testloop = botloop' B.getLine (B.putStr . (<> "\r\n"))

-- | Data required to connect to an IRC server
data IRCServer = IRCServer
    { serverConnectionParams :: ConnectionParams
    , serverPass :: Maybe Text
    , botUser :: Username
    , botRealname :: Text
    , botNickname :: Text
    }
