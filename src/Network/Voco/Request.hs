{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Network.Voco.Request
    ( Req
    , send
    , recv
    , recvG
    -- * Testing
    , testreq
    -- * Low-Level
    -- | This section is intended only for users who want to implement custom bot
    -- loops and need to handle 'Req's in some custom way.
    , stepReq
    ) where

import Control.Concurrent (forkIO)
import Control.Lens
import Control.Monad
import Control.Monad.Chan
import Control.Monad.Trans.Reader
import Data.Bifunctor
import Data.ByteString (ByteString)
import GHC.TypeLits
import Network.Voco.Action
import Network.Yak
import Network.Yak.Client

import qualified Data.ByteString as B

newtype Req a = Req
    { stepReq' :: Maybe ByteString -> ReaderT (Chan ByteString) IO (Either (Req a) a)
    }

instance Perform Req where
    perform x =
        case getAction x of
            SomeMsg m -> send m

-- | Attempt one step on a 'Req', given a channel and a possible input.
stepReq :: Chan ByteString -> Maybe ByteString -> Req a -> IO (Either (Req a) a)
stepReq c b req = runReaderT (stepReq' req b) c

instance Functor Req where
    fmap f r = Req $ \x -> bimap (fmap f) f <$> stepReq' r x

instance Applicative Req where
    pure = Req . const . pure . pure
    (<*>) = ap

instance Monad Req where
    a >>= k =
        Req $ \x -> do
            step <- stepReq' a x
            case step of
                Left r -> pure . Left $ r >>= k
                Right a' -> stepReq' (k a') Nothing

-- | Send a message in a 'Req'. Alternatively the 'Perform' instance can be
-- used to send messages as actions, defined in "Network.Voco.Aciton".
send :: (KnownSymbol c, Parameter (PList p)) => Msg c p -> Req ()
send m =
    Req $ \_ -> do
        chan <- ask
        writeChan chan (emit m)
        pure (Right ())

-- | Receive a message of a given type. The function is return type
-- polymorphic, and only messages that can be parsed into the request type will
-- further the request!
recv :: Fetch i => Req i
recv = recvG (const True)

-- | Like 'recv' but accepting an additional guard. The request will only
-- advance when the passed predicate returns true on an acceptable message.
recvG :: Fetch i => (i -> Bool) -> Req i
recvG p =
    Req $ \x ->
        pure $
        case x of
            Nothing -> Left (recvG p)
            Just x' ->
                case fetch x' of
                    Nothing -> Left $ recvG p
                    Just f ->
                        if p f
                            then Right f
                            else Left (recvG p)

-- | Testing function for 'Req' actions
testreq :: Req a -> IO a
testreq req = do
    chan <- newChan
    forkIO $ listen chan
    r <- stepReq chan Nothing req
    loop r chan
  where
    listen chan = do
        l <- readChan chan
        B.putStr l
        listen chan
    loop (Left r) chan = do
        l <- B.getLine
        r' <- stepReq chan (Just l) r
        loop r' chan
    loop (Right r) _ = pure r

test :: Req ()
test = do
    send (build Nothing ["foo!bar@quux"] :: WhoIs)
    ans <- recv
    case ans ^. privmsgMessage of
        "foo" -> message "#test" "foo!"
        "bar" -> do
            mapM_ @[] (message "#test") ["bar 1", "bar 2", "bar 3"]
            ans1 <- recv
            message "#test" (ans1 ^. privmsgMessage)
            ans2 <- recv
            message "#test" (ans2 ^. privmsgMessage)
        _ -> pure ()
