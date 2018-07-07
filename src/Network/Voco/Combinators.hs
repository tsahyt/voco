module Network.Voco.Combinators
    ( parsed
    ) where

import Data.ByteString (ByteString)
import Network.Voco.Bot
import Network.Yak

-- | Transform a bot on some fetchable IRC message (or coproduct thereof) into a
-- bot on 'ByteString', i.e. on raw IRC input.
parsed :: (Applicative m, Fetch i) => Bot m i o -> Bot m ByteString o
parsed = refine fetch
