{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.Voco.Request
    ( Req
    , send
    , recv
    , recvG
    -- * Common Requests
    , names
    -- * Testing
    , testreq
    -- * Low-Level
    -- | This section is intended only for users who want to implement custom bot
    -- loops and need to handle 'Req's in some custom way.
    , stepReq
    ) where

import Data.Coproduct
import Control.Lens (toListOf)
import Control.Concurrent (forkIO)
import Control.Monad.Chan
import Control.Monad.Loops
import GHC.TypeLits
import Network.Voco.Core
import Network.Voco.Transmit
import Network.Yak
import Network.Yak.Client
import Network.Yak.Responses

import qualified Data.ByteString as B

-- | Send a message in a 'Req'. Alternatively the 'Perform' instance can be
-- used to send messages as actions, defined in "Network.Voco.Aciton".
send :: (KnownSymbol c, Parameter (PList p)) => Msg c p -> Req ()
send = transmit . SomeMsg

-- | Receive a message of a given type. The function is return type
-- polymorphic, and only messages that can be parsed into the request type will
-- further the request!
recv :: Fetch i => Req i
recv = recvG (const True)

-- | Testing function for 'Req' actions.
testreq :: Req a -> IO a
testreq req = do
    chan <- newChan
    _ <- forkIO $ listen chan
    r <- stepReq chan Nothing req
    loop r chan
  where
    listen chan = do
        IRC l <- readChan chan
        B.putStr (emitSome l)
        listen chan
    loop (Left r) chan = do
        l <- B.getLine
        r' <- stepReq chan (Just l) r
        loop r' chan
    loop (Right r) _ = pure r

-- | Request all nicknames in a given 'Channel'
names :: Channel -> Req [Nickname]
names c = do
    send $ (build [c] :: Names)
    fmap concat . unfoldM $ do
        x <- recv :: Req (RplNamreply :|: RplEndofnames)
        case x of
            Left ns ->
                pure .
                Just . toListOf (rplNamreplyNicks . traverse . memberData) $
                ns
            Right _ -> pure Nothing
