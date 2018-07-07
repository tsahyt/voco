module Network.Voco.Combinators
    ( parsed
    , on
    , abort
    , refine
    , query
    -- * Re-exports for convenience
    , module Control.Applicative
    , module Data.Profunctor
    ) where

import Control.Applicative hiding (WrappedArrow(..))
import Data.ByteString (ByteString)
import Data.Profunctor
import Network.Voco.Bot
import Network.Yak

-- | Transform a bot on some fetchable IRC message (or coproduct thereof) into a
-- bot on 'ByteString', i.e. on raw IRC input.
parsed :: (Applicative m, Fetch i) => Bot m i o -> Bot m ByteString o
parsed = refine fetch

-- | Synonym for 'lmap'.
on :: Functor m => (i' -> i) -> Bot m i o -> Bot m i' o
on = lmap
