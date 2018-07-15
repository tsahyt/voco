{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExistentialQuantification #-}

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
    , PortNumber
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (forever)
import Control.Monad.Chan
import Control.Natural
import Data.ByteString (ByteString)
import Data.Maybe (catMaybes, maybeToList)
import Data.Monoid
import Data.Text (Text)
import Network (HostName, PortNumber)
import Network.Connection
import Network.Voco.Core
import Network.Yak
import Network.Yak.Client

import qualified Data.ByteString as B

data ReqPair =
    forall a. ReqPair (Req a)
                      (MVar a)

-- | Generalized bot loop function, taking a way to read a line from the
-- network, a way to send a line to the network, a natural transformation from
-- the underlying monad of 'Bot' to 'IO', as well as a 'Bot' of a suitable type.
botloop' ::
       MonadChan m
    => IO ByteString
    -> (ByteString -> IO ())
    -> (m :~> IO)
    -> Bot m ByteString ()
    -> IO ()
botloop' get send nt bot = do
    inChanB <- newChan
    inChanR <- dupChan inChanB
    botChan <- newChan
    reqs <- newMVar []
    _ <- forkIO $ listener inChanB
    _ <- forkIO $ shouter botChan reqs
    _ <- forkIO $ requests inChanR botChan reqs
    nt $$ forever (botprocess inChanB botChan)
  where
    listener chan =
        forever $ do
            msg <- get
            writeChan chan msg
    shouter chan reqs =
        forever $ do
            x <- readChan chan
            case x of
                IRC m -> send $ emitSome m
                RunRequest m r -> modifyMVar_ reqs (pure . (ReqPair r m :))
    requests inChan botChan rsM =
        forever $ do
            msg <- readChan inChan
            rs <- takeMVar rsM
            rs' <- catMaybes <$> mapM (processReq botChan msg) rs
            putMVar rsM rs'
    botprocess inChan botChan = do
        msg <- readChan inChan
        runBot bot botChan msg

processReq :: Chan IRCAction -> ByteString -> ReqPair -> IO (Maybe ReqPair)
processReq chan inp (ReqPair r m) = do
    x <- stepReq chan (Just inp) r
    case x of
        Left more -> pure $ Just (ReqPair more m)
        Right res -> do
            putMVar m res
            pure Nothing

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

botloop :: MonadChan m => IRCServer -> (m :~> IO) -> Bot m ByteString () -> IO ()
botloop server nt bot = do
    conn <- connectIRC server
    let get = B.init <$> connectionGetLine 4096 conn
        send x = connectionPut conn (x <> "\r\n")
    botloop' get send nt bot

testloop :: MonadChan m => (m :~> IO) -> Bot m ByteString () -> IO ()
testloop = botloop' B.getLine (B.putStr . (<> "\r\n"))

-- | Data required to connect to an IRC server
data IRCServer = IRCServer
    { serverConnectionParams :: ConnectionParams
    , serverPass :: Maybe Text
    , botUser :: Username
    , botRealname :: Text
    , botNickname :: Text
    }
