{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
module Network.Voco.Bot (
    Bot,
    -- * Bot Monad
    runBot,
    execBot,
    evalBot,
    liftBot,
    abort,
    refine,
    query,
    -- * IRC Actions
    IRCAction,
    perform
) where

import Control.Applicative
import Control.Category
import Control.Monad.Except
import Control.Monad.Random.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Bifunctor
import Data.Monoid
import Data.Profunctor
import Data.Text (Text)
import Network.Yak (SomeMsg)

import Prelude hiding ((.), id)

-- | An IRC action is simply some message that shall be sent back to the server.
type IRCAction = SomeMsg

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
    { runBot' :: i -> MaybeT m (o, [IRCAction])
    } deriving (Functor)

runBot :: Bot m i o -> i -> m (Maybe (o, [IRCAction]))
runBot b i = runMaybeT $ runBot' b i

-- | Obtain the list of actions performed by a bot.
execBot :: Functor m => Bot m i o -> i -> m (Maybe [IRCAction])
execBot b i = runMaybeT (fmap snd (runBot' b i))

-- | Obtain the final result of a bot computation, throwing away the list of
-- actions. This is usually not particularly useful, but is provided for
-- completeness.
evalBot :: Functor m => Bot m i o -> i -> m (Maybe o)
evalBot b i = runMaybeT (fmap fst (runBot' b i))

-- | 'lift' for 'Bot', since there cannot be a 'MonadTrans' instance due to the
-- argument order.
liftBot :: Monad m => m a -> Bot m i a
liftBot = Bot . const . lift . fmap (\x -> (x, []))

instance Monad m => Category (Bot m) where
    id = Bot $ \i -> pure (i, [])
    (Bot f) . (Bot g) =
        Bot $ \a -> do
            (b, as) <- g a
            second (as ++) <$> f b

instance Monad m => Applicative (Bot m i) where
    pure x = Bot $ \_ -> lift (pure (x, mempty))
    (Bot f) <*> (Bot a) =
        Bot $ \i -> do
            (f', fa) <- f i
            (a', aa) <- a i
            pure $ (f' a', fa ++ aa)

instance Monad m => Monad (Bot m i) where
    a >>= k =
        Bot $ \i -> do
            (x, xa) <- runBot' a i
            (y, ya) <- runBot' (k x) i
            pure (y, xa ++ ya)

instance MonadState s m => MonadState s (Bot m i) where
    state f = liftBot (state f)

instance MonadReader r m => MonadReader r (Bot m i) where
    ask = liftBot ask
    local f k =
        Bot $ \i ->
            let k' = runBot' k i
            in local f k'

instance MonadWriter w m => MonadWriter w (Bot m i) where
    tell x = liftBot (tell x)
    listen k =
        Bot $ \i ->
            let k' = runBot' k i
            in swapWriter $ listen k'
    pass k =
        Bot $ \i ->
            let k' = swapWriter $ runBot' k i
            in pass k'

instance MonadError e m => MonadError e (Bot m i) where
    throwError e = Bot $ \_ -> throwError e
    catchError b k =
        Bot $ \i ->
            let b' = runBot' b i
            in catchError b' (fmap (flip runBot' i) k)

instance MonadRandom m => MonadRandom (Bot m i) where
    getRandomR = liftBot . getRandomR
    getRandom = liftBot getRandom
    getRandomRs = liftBot . getRandomRs
    getRandoms = liftBot getRandoms

instance Functor m => Profunctor (Bot m) where
    lmap f (Bot k) = Bot (lmap f k)
    rmap f (Bot k) = Bot (rmap (fmap (first f)) k)

instance Functor m => Strong (Bot m) where
    first' k =
        Bot $ \(i, c) ->
            let k' = runBot' k i
            in fmap (first (, c)) k'
    second' k = 
        Bot $ \(c, i) ->
            let k' = runBot' k i
            in fmap (first (c, )) k'

instance Monad m => Choice (Bot m) where
    left' k =
        Bot $ \case
            Left l ->
                let k' = runBot' k l
                in fmap (first Left) k'
            Right r -> pure $ (Right r, [])
    right' k =
        Bot $ \case
            Left l -> pure $ (Left l, [])
            Right r -> 
                let k' = runBot' k r
                in fmap (first Right) k'

instance Monad m => Alternative (Bot m i) where
    empty = Bot . const . MaybeT . pure $ Nothing
    (Bot a) <|> (Bot b) =
        Bot $ \i ->
            MaybeT $ do
                a' <- runMaybeT $ a i
                case a' of
                    Nothing -> runMaybeT $ b i
                    Just y -> pure a'

instance (Monad m, Monoid o) => Monoid (Bot m i o) where
    mempty = empty
    mappend a b = getAlt $ Alt a <> Alt b

-- | Helper function for 'MonadWriter' implementation
swapWriter :: Functor f => f ((a, x), w) -> f ((a, w), x)
swapWriter = fmap $ \((a, x), w) -> ((a, w), x)
{-# INLINE swapWriter #-}

-- | 'empty' without the 'Monad' constraint required by '(<|>)'.
abort :: Applicative m => Bot m i a
abort = Bot $ \_ -> MaybeT $ pure Nothing

-- | Refine the input to a bot, possibly failing. Like 'lmap' but with a
-- computation that can fail. Useful e.g. for applying a parser to an input.
refine :: Applicative m => (a -> Maybe b) -> Bot m b c -> Bot m a c
refine f (Bot k) =
    Bot $ \i ->
        case f i of
            Nothing -> MaybeT $ pure Nothing
            Just x -> k x

-- | Obtain the input to a bot.
query :: Monad m => Bot m i i
query = Bot $ \i -> pure (i, [])

-- | Perform an 'IRCAction'. For most uses, the convenience functions in
-- "Network.Voco.Action" are preferable.
perform :: Monad m => IRCAction -> Bot m i ()
perform a = Bot $ \_ -> pure ((), [a])
