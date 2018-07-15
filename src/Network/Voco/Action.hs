{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Covenience functions to perform 'IRCAction's in the bot monad.
module Network.Voco.Action
    ( IRCAction(..)
    , Perform(..)
    , pong
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

import Control.Monad.Chan
import Data.Text (Text)
import Data.Monoid
import Data.List.NonEmpty
import Network.Yak.Client hiding (nick)
import Network.Yak.Types

-- | An IRC action is simply some message that shall be sent back to the server.
newtype IRCAction = IRCAction { getAction :: SomeMsg }

class Perform m where
    perform :: IRCAction -> m ()

pong :: Perform m => Hostname -> Maybe Hostname -> m ()
pong h1 h2 = perform . IRCAction $ SomeMsg (build h1 h2 :: Pong)

user :: Perform m => Username -> Text -> m ()
user u r = perform . IRCAction $ SomeMsg (build u 0 Unused (Message r) :: User)

pass :: Perform m => Text -> m ()
pass p = perform . IRCAction $ SomeMsg (build p :: Pass)

nickservIdentify :: Perform m => Text -> m ()
nickservIdentify p = messageUser "NickServ" (Message $ "IDENTIFY " <> p)

message :: Perform m => Channel -> Message -> m ()
message c m = perform . IRCAction $ SomeMsg (build [Left c] m :: Privmsg)

messageUser :: Perform m => Nickname -> Message -> m ()
messageUser n m = perform . IRCAction $ SomeMsg (build [Right n] m :: Privmsg)

notice :: Perform m => Channel -> Message -> m ()
notice c m = perform . IRCAction $ SomeMsg (build [Left c] m :: Notice)

noticeUser :: Perform m => Nickname -> Message -> m ()
noticeUser n m = perform . IRCAction $ SomeMsg (build [Right n] m :: Notice)

join :: Perform m => NonEmpty Channel -> m ()
join cs = perform . IRCAction $ SomeMsg (build cs [] :: Join)

join' :: Perform m => Channel -> m ()
join' x = join [x]

part :: Perform m => NonEmpty Channel -> Maybe Message -> m ()
part cs m = perform . IRCAction $ SomeMsg (build cs m :: Part)

part' :: Perform m => Channel -> Maybe Message -> m ()
part' x = part [x]

nick :: Perform m => Nickname -> m ()
nick n = perform . IRCAction $ SomeMsg (build n :: Nick)

kick :: Perform m => Channel -> Nickname -> Maybe Message -> m ()
kick c n m = perform . IRCAction $ SomeMsg (build [c] [n] m :: Kick)

invite :: Perform m => Nickname -> Channel -> m ()
invite n c = perform . IRCAction $ SomeMsg (build n c :: Invite)
