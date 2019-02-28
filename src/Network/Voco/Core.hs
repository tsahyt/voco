{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Network.Voco.Core
    ( -- * Bot Monad
      Bot
    , runBot
    , liftBot
    -- ** Basic Combinators
    , abort
    , refine
    , query
    , divide
    , natural
    , async
    -- * Requests
    , request
    , Req
    , stepReq
    , recvG
    -- * Actions
    , IRCAction(..)
    ) where

import Control.Arrow
import Control.Applicative
import Control.Category
import Control.Concurrent.Async (Async)
import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Control.Monad.Chan
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Random.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Control.Natural
import Data.Bifunctor
import Data.Profunctor
import Network.Yak
import Network.Voco.Transmit (Transmit(..))

import qualified Control.Concurrent.Async as A
import Prelude hiding ((.), id)

-- | An IRC action is simply some message that shall be sent back to the server.
data IRCAction where
    IRC :: SomeMsg -> IRCAction
    RunRequest :: MVar a -> Req a -> IRCAction

-- | The bot abstraction provides a composable way to define IRC bots. A bot is
-- parameterized over three types. 
--
-- * @m@ defines the underlying monad. This allows using state, or some
-- read-only environment. This environment is /preserved between invocations/ in
-- the 'botloop' family of functions. 
--
-- * @i@ defines the type a particular 'Bot' takes as input. A "top-level" bot
-- will always require 'ByteString' input. Bots are profunctors, the input can
-- therefore be adjusted as necessary. The 'parsed' combinator for instance
-- provides an easy way to adjust the input to some IRC message (as defined in
-- "Network.Yak").
--
-- * @o@ defines the wrapped value of this bot computation. This does /not/
-- define the resulting actions performed in a command. See
-- "Network.Voco.Action" or 'perform'.
--
-- For composition, bots are monads, categories, and (strong) profunctors. Most
-- importantly bots can fail, and provide an 'Alternative' (and 'Monoid')
-- instance.
--
-- For bots working on multiple possible inputs, 'Either' can be used. See
-- "Data.Coproduct" for more.
newtype Bot m i o = Bot
    { runBot' :: Chan IRCAction -> i -> MaybeT m o
    } deriving (Functor)

-- | Obtain the final result of a bot computation
runBot :: Bot m i o -> Chan IRCAction -> i -> m (Maybe o)
runBot b c i = runMaybeT $ runBot' b c i

-- | 'lift' for 'Bot', since there cannot be a 'MonadTrans' instance due to the
-- argument order.
liftBot :: Monad m => m a -> Bot m i a
liftBot x = Bot $ \_ _ -> lift x

instance Monad m => Category (Bot m) where
    id = Bot $ \_ i -> pure i
    (Bot f) . (Bot g) = Bot $ \c -> g c >=> f c

instance Monad m => Arrow (Bot m) where
    arr f = query >>= \b -> pure $ f b
    first = first'

instance Monad m => ArrowZero (Bot m) where
    zeroArrow = empty

-- | Alternative semantics
instance Monad m => ArrowPlus (Bot m) where
    (<+>) = (<|>)

instance Monad m => ArrowChoice (Bot m) where
    left = left'

instance Monad m => ArrowApply (Bot m) where
    app = Bot $ \c (bt, b) -> runBot' bt c b

instance Monad m => Applicative (Bot m i) where
    pure x = Bot $ \_ _ -> MaybeT . pure . Just $ x
    a <*> b = Bot $ \c i -> runBot' a c i <*> runBot' b c i

instance Monad m => Monad (Bot m i) where
    a >>= k =
        Bot $ \c i -> do
            x <- runBot' a c i
            y <- runBot' (k x) c i
            pure y

instance MonadState s m => MonadState s (Bot m i) where
    state f = liftBot (state f)

instance MonadReader r m => MonadReader r (Bot m i) where
    ask = liftBot ask
    local f k = Bot $ \c -> local f . runBot' k c

instance MonadWriter w m => MonadWriter w (Bot m i) where
    tell x = liftBot (tell x)
    listen k = Bot $ \c -> listen . runBot' k c
    pass k = Bot $ \c -> pass . runBot' k c

instance MonadError e m => MonadError e (Bot m i) where
    throwError e = Bot $ \_ _ -> throwError e
    catchError a h =
        Bot $ \c i -> catchError (runBot' a c i) (fmap (\x -> runBot' x c i) h)

instance MonadRandom m => MonadRandom (Bot m i) where
    getRandomR = liftBot . getRandomR
    getRandom = liftBot getRandom
    getRandomRs = liftBot . getRandomRs
    getRandoms = liftBot getRandoms

instance MonadIO m => MonadIO (Bot m i) where
    liftIO = liftBot . liftIO

instance MonadLogger m => MonadLogger (Bot m i) where
    monadLoggerLog loc source level m =
        liftBot $ monadLoggerLog loc source level m

instance Functor m => Profunctor (Bot m) where
    lmap f (Bot k) = Bot $ \c -> (lmap f (k c))
    rmap f (Bot k) = Bot $ \c -> (rmap (fmap f) (k c))

instance Functor m => Strong (Bot m) where
    first' k =
        Bot $ \chan (i, c) ->
            let k' = runBot' k chan i
            in (, c) <$> k'
    second' k =
        Bot $ \chan (c, i) ->
            let k' = runBot' k chan i
            in (c, ) <$> k'

instance Monad m => Choice (Bot m) where
    left' k =
        Bot $ \c ->
            \case
                Left l ->
                    let k' = runBot' k c l
                    in fmap Left k'
                Right r -> pure $ Right r
    right' k =
        Bot $ \c ->
            \case
                Left l -> pure $ Left l
                Right r ->
                    let k' = runBot' k c r
                    in fmap Right k'

-- | First non-failing result is returned.
instance Monad m => Alternative (Bot m i) where
    empty = Bot $ \_ _ -> MaybeT . pure $ Nothing
    (Bot a) <|> (Bot b) =
        Bot $ \c i ->
            MaybeT $ do
                a' <- runMaybeT $ a c i
                case a' of
                    Nothing -> runMaybeT $ b c i
                    Just _ -> pure a'

instance (Monad m, Semigroup o) => Semigroup (Bot m i o) where
    a <> b =
        Bot $ \c i ->
            MaybeT $ do
                ares <- runBot a c i
                case ares of
                    Nothing -> runBot b c i
                    Just ares' -> do
                        bres <- runBot b c i
                        case bres of
                            Nothing -> pure ares
                            Just bres' -> pure . Just $ ares' <> bres'

-- | Collect all non-failing results.
instance (Monad m, Monoid o) => Monoid (Bot m i o) where
    mempty = empty
    mappend = (<>)

instance MonadChan m => Transmit (Bot m i) where
    transmit m = perform (IRC m)

perform :: MonadChan m => IRCAction -> Bot m i ()
perform a = Bot $ \c _ -> lift (writeChan c a)

-- | 'empty' without the 'Monad' constraint required by '(<|>)'.
abort :: Applicative m => Bot m i a
abort = Bot $ \_ _ -> MaybeT $ pure Nothing

-- | Refine the input to a bot, possibly failing. Like 'lmap' but with a
-- computation that can fail. Useful e.g. for applying a parser to an input.
refine :: Applicative m => (a -> Maybe b) -> Bot m b c -> Bot m a c
refine f (Bot k) =
    Bot $ \c i ->
        case f i of
            Nothing -> MaybeT $ pure Nothing
            Just x -> k c x

-- | Obtain the input to a bot.
query :: Monad m => Bot m i i
query = id

natural :: (m :~> n) -> Bot m i o -> Bot n i o
natural nat b = Bot $ \c i -> MaybeT (nat $$ runBot b c i)

-- | Run the sub-bot asynchronously. Note that the sub-bot is run with 'IO' as
-- its underlying monad. You can use e.g. 'natural' to rebuild some custom
-- stack, and you can of course introduce data from the surrounding environment
-- in a closure. However, it is not possible to carry on the existing stack on
-- the outside into the asynchronous computation, which could lead to seriously
-- strange behaviour. You are therefore asked to make your intentions explicit.
--
-- The resulting bot returns an 'Async', which can be stored away and waited on,
-- or killed. Consult "Control.Concurrent.Async" for details. If this is not
-- needed, be sure to not let the value linger around in scope, such that the
-- thread can be garbage collected after execution.
async :: MonadIO m => Bot IO i a -> Bot m i (Async (Maybe a))
async b = Bot $ \c -> liftIO . A.async . runBot b c

-- | Divide and conquer for bots, analogous to the
-- "Data.Functor.Contravariant.Divisible" module. Due to argument ordering it is
-- not possible to implement the class. 'conquer' is given as some application
-- of 'pure'. We need to assume some way to combine outputs, hence the 'Monoid'
-- constraint.
divide ::
       (Monad m, Monoid o)
    => (i -> (j, k))
    -> Bot m j o
    -> Bot m k o
    -> Bot m i o
divide f l r =
    Bot $ \c i -> do
        let (j, k) = f i
        l' <- runBot' l c j
        r' <- runBot' r c k
        pure $ l' <> r'

bootstrapReq :: MonadIO m => Req a -> Bot m i (Either (Req a) a)
bootstrapReq req =
    Bot $ \c _ -> MaybeT . fmap Just . liftIO . stepReq c Nothing $ req

-- | Run a 'Req' in a bot. Note that this will block until the 'Req' has been
-- processed entirely, and should therefore only be used inside an asynchronous
-- bot. See 'async' for creation of asynchronous bots.
request :: (MonadIO m, MonadChan m) => Req a -> Bot m i a
request req = bootstrapReq req >>= \case
    Left req' -> do
        mvar <- liftIO newEmptyMVar
        perform $ RunRequest mvar req'
        liftIO $ takeMVar mvar
    Right a -> pure a

newtype Req a = Req
    { stepReq' :: Maybe ByteString -> ReaderT (Chan IRCAction) IO (Either (Req a) a)
    }

instance Transmit Req where
    transmit m =
        Req $ \_ -> do
            chan <- ask
            writeChan chan (IRC m)
            pure (Right ())

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

-- | Attempt one step on a 'Req', given a channel and a possible input.
stepReq :: Chan IRCAction -> Maybe ByteString -> Req a -> IO (Either (Req a) a)
stepReq c b req = runReaderT (stepReq' req b) c

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
