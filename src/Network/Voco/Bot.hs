{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Network.Voco.Bot (
    Bot,
    -- * Bot Monad
    runBot,
    liftBot,
    -- ** Basic Combinators
    abort,
    refine,
    query,
    natural,
    divide,
    async,
    -- * IRC Actions
    IRCAction (..),
    perform
) where

import Control.Applicative
import Control.Category
import Control.Concurrent.Classy
import Control.Concurrent.Classy.Async (Async)
import qualified Control.Concurrent.Classy.Async as A
import Control.Lens.Zoom
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Random.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Control.Natural
import Data.Bifunctor
import Data.Maybe (maybeToList)
import Data.Monoid
import Data.Profunctor
import Data.Text (Text)
import Network.Yak (SomeMsg)

import Prelude hiding ((.), id)

-- | An IRC action is simply some message that shall be sent back to the server.
-- You generally do not need nor want to construct these values directly. See
-- "Network.Voco.Action".
data IRCAction = IRCAction SomeMsg

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
    { runBot' :: i -> MaybeT m o
    } deriving (Functor)

-- | Obtain the final result of a bot computation
runBot :: Bot m i o -> i -> m (Maybe o)
runBot b i = runMaybeT $ runBot' b i

-- | 'lift' for 'Bot', since there cannot be a 'MonadTrans' instance due to the
-- argument order.
liftBot :: Monad m => m a -> Bot m i a
liftBot = Bot . const . lift

instance Monad m => Category (Bot m) where
    id = Bot $ \i -> pure i
    (Bot f) . (Bot g) = Bot $ g >=> f

instance Monad m => Applicative (Bot m i) where
    pure x = Bot $ \_ -> MaybeT . pure . Just $ x
    a <*> b = Bot $ \i -> runBot' a i <*> runBot' b i

instance Monad m => Monad (Bot m i) where
    a >>= k =
        Bot $ \i -> do
            x <- runBot' a i
            y <- runBot' (k x) i
            pure y

instance MonadState s m => MonadState s (Bot m i) where
    state f = liftBot (state f)

instance MonadReader r m => MonadReader r (Bot m i) where
    ask = liftBot ask
    local f k = Bot $ local f . runBot' k

instance MonadWriter w m => MonadWriter w (Bot m i) where
    tell x = liftBot (tell x)
    listen k = Bot $ listen . runBot' k
    pass k = Bot $ pass . runBot' k

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

instance MonadIO m => MonadIO (Bot m i) where
    liftIO = liftBot . liftIO

instance MonadLogger m => MonadLogger (Bot m i) where
    monadLoggerLog loc source level m =
        liftBot $ monadLoggerLog loc source level m

instance Functor m => Profunctor (Bot m) where
    lmap f (Bot k) = Bot (lmap f k)
    rmap f (Bot k) = Bot (rmap (fmap f) k)

instance Functor m => Strong (Bot m) where
    first' k =
        Bot $ \(i, c) ->
            let k' = runBot' k i
            in (, c) <$> k'
    second' k =
        Bot $ \(c, i) ->
            let k' = runBot' k i
            in (c, ) <$> k'

instance Monad m => Choice (Bot m) where
    left' k =
        Bot $ \case
            Left l ->
                let k' = runBot' k l
                in fmap Left k'
            Right r -> pure $ Right r
    right' k =
        Bot $ \case
            Left l -> pure $ Left l
            Right r -> 
                let k' = runBot' k r
                in fmap Right k'

-- | First non-failing result is returned.
instance Monad m => Alternative (Bot m i) where
    empty = Bot . const . MaybeT . pure $ Nothing
    (Bot a) <|> (Bot b) =
        Bot $ \i ->
            MaybeT $ do
                a' <- runMaybeT $ a i
                case a' of
                    Nothing -> runMaybeT $ b i
                    Just y -> pure a'

-- | Collect all non-failing results.
instance (Monad m, Monoid o) => Monoid (Bot m i o) where
    mempty = empty
    mappend a b =
        Bot $ \i ->
            MaybeT $ do
                ares <- runBot a i
                case ares of
                    Nothing -> runBot b i
                    Just ares' -> do
                        bres <- runBot b i
                        case bres of
                            Nothing -> pure ares
                            Just bres' -> pure . Just $ ares' <> bres'

type instance Zoomed (Bot m i) =
     Zoomed (ReaderT i (MaybeT m))

instance Zoom m n s t => Zoom (Bot m i) (Bot n i) s t where
    -- Implemented via roundtrip to monad transformers. In reality this costs
    -- only the fmap, since everything else is newtypes.
    zoom l x = stackToBot $$ zoom l (botToStack $$ x)
      where
        stackToBot = NT (Bot . runReaderT)
        botToStack = NT (ReaderT . runBot')

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
query = id

-- | Perform an 'IRCAction'. For most uses, the convenience functions in
-- "Network.Voco.Action" are preferable.
perform :: MonadConc m => IRCAction -> Bot m i ()
perform a = undefined -- Bot $ \_ -> pure ((), [a])

-- | Apply a natural transformation to the underlying monad of a bot
natural :: (m :~> n) -> Bot m i o -> Bot n i o
natural nt b = Bot $ MaybeT . (nt $$) . runMaybeT . runBot' b 

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
--
-- All 'IRCAction's performed in the sub-bot will be executed /at the end of its
-- execution/!
--
-- Note that 'async' can be nested.
async :: MonadIO m => Bot IO i a -> Bot m i (Async m (Maybe a))
async b = undefined
    {-
     -Bot $ \i ->
     -    liftIO $ do
     -        future <- A.async $ runBot b i
     -        let acts = concat . maybeToList . fmap snd <$> future
     -            res  = fmap fst <$> future
     -        pure (res, [FutureAction acts])
     -}

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
    Bot $ \i -> do
        let (j, k) = f i
        l' <- runBot' l j
        r' <- runBot' r k
        pure $ l' <> r'
