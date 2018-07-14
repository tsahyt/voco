{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Covenience functions to perform 'IRCAction's in the bot monad.
module Network.Voco.Action
    ( pong
    , user
    , pass
    , nickservIdentify
    , message
    , messageUser
    , notice
    , noticeUser
    , join
    , join'
    , part
    , part'
    , nick
    ) where

import Control.Concurrent.Classy (MonadConc)
import Data.Text (Text)
import Data.Monoid
import Data.List.NonEmpty
import Network.Voco.Bot
import Network.Yak.Client hiding (nick)
import Network.Yak.Types

pong :: MonadConc m => Hostname -> Maybe Hostname -> Bot m i ()
pong h1 h2 = perform . IRCAction $ SomeMsg (build h1 h2 :: Pong)

user :: MonadConc m => Username -> Text -> Bot m i ()
user u r = perform . IRCAction $ SomeMsg (build u 0 Unused (Message r) :: User)

pass :: MonadConc m => Text -> Bot m i ()
pass p = perform . IRCAction $ SomeMsg (build p :: Pass)

nickservIdentify :: MonadConc m => Text -> Bot m i ()
nickservIdentify p = messageUser "NickServ" (Message $ "IDENTIFY " <> p)

message :: MonadConc m => Channel -> Message -> Bot m i ()
message c m = perform . IRCAction $ SomeMsg (build [Left c] m :: Privmsg)

messageUser :: MonadConc m => Nickname -> Message -> Bot m i ()
messageUser n m = perform . IRCAction $ SomeMsg (build [Right n] m :: Privmsg)

notice :: MonadConc m => Channel -> Message -> Bot m i ()
notice c m = perform . IRCAction $ SomeMsg (build [Left c] m :: Notice)

noticeUser :: MonadConc m => Nickname -> Message -> Bot m i ()
noticeUser n m = perform . IRCAction $ SomeMsg (build [Right n] m :: Notice)

join :: MonadConc m => NonEmpty Channel -> Bot m i ()
join cs = perform . IRCAction $ SomeMsg (build cs [] :: Join)

join' :: MonadConc m => Channel -> Bot m i ()
join' x = join [x]

part :: MonadConc m => NonEmpty Channel -> Maybe Message -> Bot m i ()
part cs m = perform . IRCAction $ SomeMsg (build cs m :: Part)

part' :: MonadConc m => Channel -> Maybe Message -> Bot m i ()
part' x = part [x]

nick :: MonadConc m => Nickname -> Bot m i ()
nick n = perform . IRCAction $ SomeMsg (build n :: Nick)

kick :: MonadConc m => Channel -> Nickname -> Maybe Message -> Bot m i ()
kick c n m = perform . IRCAction $ SomeMsg (build [c] [n] m :: Kick)

invite :: MonadConc m => Nickname -> Channel -> Bot m i ()
invite n c = perform . IRCAction $ SomeMsg (build n c :: Invite)
