{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Network.Voco.IO (
    botloop,
    PortID (..),
    IRCServer(..),
    -- * Low-Level
    runActions,
    botloop',
    -- * Testing
    testloop
) where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.IO.Class
import Control.Natural
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Network
import Network.Voco.Bot
import Network.Yak
import Network.Yak.Client
import System.IO

import qualified Data.ByteString as B

import Debug.Trace

-- | Run a given list of IRC actions, using a specified send function.
runActions :: (ByteString -> IO ()) -> [IRCAction] -> IO ()
runActions send = mapM_ (send . emitSome)

-- | Generalized bot loop function, taking a way to read a line from the
-- network, a way to send a line to the network, a natural transformation from
-- the underlying monad of 'Bot' to 'IO', as well as a 'Bot' of a suitable type.
--
-- This loop treats each input asynchronously.
botloop' ::
       MonadIO m
    => IO ByteString
    -> (ByteString -> IO ())
    -> (m :~> IO)
    -> Bot m ByteString ()
    -> IO ()
botloop' input send nt bot = do
    out <- newChan
    let printer = do
            msg <- readChan out
            send msg
            printer
        loop = do
            i <- liftIO $ input
            ans <- execBot bot i
            case ans of
                Nothing -> loop
                Just ks -> liftIO (runActions (writeChan out) ks) *> loop
    forkIO $ printer
    nt $$ loop

connectIRC :: MonadIO m => IRCServer -> m Handle
connectIRC server =
    liftIO $ do
        h <- connectTo (serverHost server) (serverPort server)
        hSetBuffering h NoBuffering
    -- bootstrapping procedure
        runActions (writeIRC h) $
            -- send password if given
            [SomeMsg (build p :: Pass) | p <- maybeToList (serverPass server)] ++
            -- send user and nick
            [ SomeMsg
                  (build
                       (botUser server)
                       0
                       Unused
                       (Message $ botRealname server) :: User)
            , SomeMsg (build (botNickname server) :: Nick)
            ]
        return h

readIRC :: MonadIO m => Handle -> m ByteString
readIRC h = do
    x <- liftIO (B.init <$> B.hGetLine h)
    traceShowM ("in", x)
    pure x

writeIRC :: MonadIO m => Handle -> ByteString -> m ()
writeIRC h x = do
    traceShowM ("out", x)
    liftIO $ B.hPutStr h (x <> "\r\n")

-- | Standard bot loop function to work with an IRC server specified as a
-- 'IRCServer' value, a natural transformation, and a bot.
botloop :: MonadIO m => IRCServer -> (m :~> IO) -> Bot m ByteString () -> IO ()
botloop server nt bot = do
    h <- connectIRC server
    botloop' (readIRC h) (writeIRC h) nt bot

-- | A testing function, mocking a bot on stdin/stdout.
testloop :: MonadIO m => (m :~> IO) -> Bot m ByteString () -> IO ()
testloop nt bot = botloop' (readIRC stdin) (writeIRC stdout) nt bot

-- | Data required to connect to an IRC server
data IRCServer = IRCServer
    { serverHost :: HostName
    , serverPort :: PortID
    , serverPass :: Maybe Text
    , botUser :: Username
    , botRealname :: Text
    , botNickname :: Text
    } deriving (Eq, Show)
