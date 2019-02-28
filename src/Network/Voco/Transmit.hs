{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

-- | Covenience functions to transmit IRC messages to the server in the bot
-- monad.
module Network.Voco.Transmit
    ( Transmit(..)
    , pong
    , user
    , pass
    , nickservIdentify
    , message
    , messageUser
    , message'
    , notice
    , noticeUser
    , join
    , join'
    , part
    , part'
    , nick
    , kick
    , invite
    ) where

import Data.Text (Text)
import Data.List.NonEmpty
import Network.Yak.Client hiding (nick)
import Network.Yak.Types

class Transmit m where
    transmit :: SomeMsg -> m ()

pong :: Transmit m => Hostname -> Maybe Hostname -> m ()
pong h1 h2 = transmit $ SomeMsg (build h1 h2 :: Pong)

user :: Transmit m => Username -> Text -> m ()
user u r = transmit $ SomeMsg (build u 0 Unused (Message r) :: User)

pass :: Transmit m => Text -> m ()
pass p = transmit $ SomeMsg (build p :: Pass)

nickservIdentify :: Transmit m => Text -> m ()
nickservIdentify p = messageUser "NickServ" (Message $ "IDENTIFY " <> p)

message' :: Transmit m => Either Channel Nickname -> Message -> m ()
message' t m = transmit $ SomeMsg (build [t] m :: Privmsg)

message :: Transmit m => Channel -> Message -> m ()
message c m = message' (Left c) m

messageUser :: Transmit m => Nickname -> Message -> m ()
messageUser n m = message' (Right n) m

notice :: Transmit m => Channel -> Message -> m ()
notice c m = transmit $ SomeMsg (build [Left c] m :: Notice)

noticeUser :: Transmit m => Nickname -> Message -> m ()
noticeUser n m = transmit $ SomeMsg (build [Right n] m :: Notice)

join :: Transmit m => NonEmpty Channel -> m ()
join cs = transmit $ SomeMsg (build cs [] :: Join)

join' :: Transmit m => Channel -> m ()
join' x = join [x]

part :: Transmit m => NonEmpty Channel -> Maybe Message -> m ()
part cs m = transmit $ SomeMsg (build cs m :: Part)

part' :: Transmit m => Channel -> Maybe Message -> m ()
part' x = part [x]

nick :: Transmit m => Nickname -> m ()
nick n = transmit $ SomeMsg (build n :: Nick)

kick :: Transmit m => Channel -> Nickname -> Maybe Message -> m ()
kick c n m = transmit $ SomeMsg (build [c] [n] m :: Kick)

invite :: Transmit m => Nickname -> Channel -> m ()
invite n c = transmit $ SomeMsg (build n c :: Invite)
