{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Voco.Request
    ( Req
    , stepReq
    , send
    , recv
    , recvG
    -- * Testing
    , reqtestloop
    ) where

import Control.Concurrent (forkIO)
import Control.Lens
import Control.Monad
import Control.Monad.Chan
import Control.Monad.Trans.Reader
import Data.Bifunctor
import Data.ByteString (ByteString)
import GHC.TypeLits
import Network.Yak
import Network.Yak.Client

import qualified Data.ByteString as B

newtype Req a = Req
    { stepReq' :: Maybe ByteString -> ReaderT (Chan ByteString) IO (Either (Req a) a)
    }

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

-- | Send a message in a 'Req'.
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
reqtestloop :: Req a -> IO a
reqtestloop req = do
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
        "foo" -> send (build [Left $ Channel "#test"] "foo!" :: Privmsg)
        "bar" -> do
            send (build [Left $ Channel "#test"] "bar 1" :: Privmsg)
            send (build [Left $ Channel "#test"] "bar 2" :: Privmsg)
            send (build [Left $ Channel "#test"] "bar 3" :: Privmsg)
            ans1 <- recv
            send
                (build [Left $ Channel "#test"] (ans1 ^. privmsgMessage) :: Privmsg)
            ans2 <- recv
            send
                (build [Left $ Channel "#test"] (ans2 ^. privmsgMessage) :: Privmsg)
        _ -> pure ()

