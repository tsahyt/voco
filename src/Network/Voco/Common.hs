-- | Module defining common or even ubiquitous bots.
module Network.Voco.Common (
) where

import Control.Lens
import Network.Voco.Bot
import Network.Voco.Combinators
import Network.Voco.Action
import Network.Yak.Client

-- | A bot responding to ping.
pingpong :: Monad m => Bot m Ping ()
pingpong = do
    ping <- query
    pong (ping ^. pingServer1) (ping ^. pingServer2)
