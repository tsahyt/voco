module Control.Monad.Chan (
    C.Chan,
    MonadChan(..)
) where

import qualified Control.Concurrent as C
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Identity

import qualified Control.Monad.Random.Lazy as RL
import qualified Control.Monad.Random.Strict as RS

-- | Simple polymorphic interface for monads that support channel operations as
-- defined in "Control.Concurrent.Chan" without requiring an explicit IO type
-- or too general 'MonadIO' constraint.
--
-- 'Bot's use this over the more general methods provided in the concurrency
-- package since it still allows natural transformations on monad stacks,
-- because the underlying channel type is not bound to the monad stack.
class Monad m => MonadChan m where
    newChan :: m (C.Chan a)
    writeChan :: C.Chan a -> a -> m ()
    readChan :: C.Chan a -> m a
    dupChan :: C.Chan a -> m (C.Chan a)

instance MonadChan IO where
    newChan = C.newChan
    writeChan = C.writeChan
    readChan = C.readChan
    dupChan = C.dupChan

instance MonadChan m => MonadChan (ReaderT r m) where
    newChan = lift newChan
    writeChan c a = lift $ writeChan c a
    readChan = lift . readChan
    dupChan = lift . dupChan

instance MonadChan m => MonadChan (StateT s m) where
    newChan = lift newChan
    writeChan c a = lift $ writeChan c a
    readChan = lift . readChan
    dupChan = lift . dupChan

instance (Monoid w, MonadChan m) => MonadChan (WriterT w m) where
    newChan = lift newChan
    writeChan c a = lift $ writeChan c a
    readChan = lift . readChan
    dupChan = lift . dupChan

instance MonadChan m => MonadChan (LoggingT m) where
    newChan = lift newChan
    writeChan c a = lift $ writeChan c a
    readChan = lift . readChan
    dupChan = lift . dupChan

instance MonadChan m => MonadChan (WriterLoggingT m) where
    newChan = lift newChan
    writeChan c a = lift $ writeChan c a
    readChan = lift . readChan
    dupChan = lift . dupChan

instance MonadChan m => MonadChan (NoLoggingT m) where
    newChan = lift newChan
    writeChan c a = lift $ writeChan c a
    readChan = lift . readChan
    dupChan = lift . dupChan

instance MonadChan m => MonadChan (ExceptT e m) where
    newChan = lift newChan
    writeChan c a = lift $ writeChan c a
    readChan = lift . readChan
    dupChan = lift . dupChan

instance MonadChan m => MonadChan (MaybeT m) where
    newChan = lift newChan
    writeChan c a = lift $ writeChan c a
    readChan = lift . readChan
    dupChan = lift . dupChan

instance (Monoid w, MonadChan m) => MonadChan (RWST r w s m) where
    newChan = lift newChan
    writeChan c a = lift $ writeChan c a
    readChan = lift . readChan
    dupChan = lift . dupChan

instance MonadChan m => MonadChan (IdentityT m) where
    newChan = lift newChan
    writeChan c a = lift $ writeChan c a
    readChan = lift . readChan
    dupChan = lift . dupChan

instance MonadChan m => MonadChan (RL.RandT g m) where
    newChan = lift newChan
    writeChan c a = lift $ writeChan c a
    readChan = lift . readChan
    dupChan = lift . dupChan

instance MonadChan m => MonadChan (RS.RandT g m) where
    newChan = lift newChan
    writeChan c a = lift $ writeChan c a
    readChan = lift . readChan
    dupChan = lift . dupChan
