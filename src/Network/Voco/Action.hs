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

import Data.Text (Text)
import Data.Monoid
import Data.List.NonEmpty
import Network.Voco.Bot
import Network.Yak.Client
import Network.Yak.Types

pong :: Monad m => Hostname -> Maybe Hostname -> Bot m i ()
pong h1 h2 = perform $ SomeMsg (build h1 h2 :: Pong)

user :: Monad m => Username -> Text -> Bot m i ()
user u r = perform $ SomeMsg (build u 0 Unused (Message r) :: User)

pass :: Monad m => Text -> Bot m i ()
pass p = perform $ SomeMsg (build p :: Pass)

nickservIdentify :: Monad m => Text -> Bot m i ()
nickservIdentify p = messageUser "NickServ" (Message $ "IDENTIFY " <> p)

message :: Monad m => Channel -> Message -> Bot m i ()
message c m = perform $ SomeMsg (build [Left c] m :: Privmsg)

messageUser :: Monad m => Nickname -> Message -> Bot m i ()
messageUser n m = perform $ SomeMsg (build [Right n] m :: Privmsg)

notice :: Monad m => Channel -> Message -> Bot m i ()
notice c m = perform $ SomeMsg (build [Left c] m :: Notice)

noticeUser :: Monad m => Nickname -> Message -> Bot m i ()
noticeUser n m = perform $ SomeMsg (build [Right n] m :: Notice)

join :: Monad m => NonEmpty Channel -> Bot m i ()
join cs = perform $ SomeMsg (build cs [] :: Join)

join' :: Monad m => Channel -> Bot m i ()
join' x = join [x]

part :: Monad m => NonEmpty Channel -> Maybe Message -> Bot m i ()
part cs m = perform $ SomeMsg (build cs m :: Part)

part' :: Monad m => Channel -> Maybe Message -> Bot m i ()
part' x = part [x]

nick :: Monad m => Nickname -> Bot m i ()
nick n = perform $ SomeMsg (build n :: Nick)

kick :: Monad m => Channel -> Nickname -> Maybe Message -> Bot m i ()
kick c n m = perform $ SomeMsg (build [c] [n] m :: Kick)

invite :: Monad m => Nickname -> Channel -> Bot m i ()
invite n c = perform $ SomeMsg (build n c :: Invite)
