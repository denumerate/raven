{-# LANGUAGE
  DeriveAnyClass
, DeriveGeneric
#-}
module Raven.Server.NodeMsgs
  ( REPLMsg(..)
  , ProcessedMsg(..)
  , KillMsg(..)
  , LogMsg(..)
  , LoginMsg(..)
  , LoginSucMsg(..)
  , NewTokenMsg(..)
  , LogoutMsg(..)
  , buildLogMsg
  , catchAllMsgs
  , catchAllMsgs'
  ) where

import Control.Distributed.Process
import Control.Concurrent

import Data.Binary
import Data.Typeable
import Data.Time.Clock
import GHC.Generics
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)

data REPLMsg = REPLMsg ByteString String
  deriving (Generic,Binary,Typeable)

data ProcessedMsg = ProcessedMsg ByteString String
  deriving (Generic,Binary,Typeable)

data KillMsg = KillMsg ByteString
  deriving (Generic,Binary,Typeable)

-- |LogMsg holds: the message, processId, timestamp
data LogMsg = LogMsg String ProcessId String
  deriving (Generic,Binary,Typeable)

-- |LoginMsg holds: line,username,password
data LoginMsg = LoginMsg ByteString Text Text
  deriving (Generic,Binary,Typeable)

-- |LoginSuccMsg holds: (id,access)
data LoginSucMsg = LoginSucMsg (Text,Bool)
 deriving (Generic,Binary,Typeable)

-- |NewTokenMsg holds: (Token, id)
data NewTokenMsg = NewTokenMsg (Text,Text)
  deriving (Generic,Binary,Typeable)

data LogoutMsg = LogoutMsg ByteString
  deriving (Generic,Binary,Typeable)

-- |Standardized LogMsg
buildLogMsg :: String -> Process LogMsg
buildLogMsg msg = getSelfPid >>=
  (\self -> liftIO getCurrentTime >>=
    (\time -> return (LogMsg msg self (show time))))

-- |Final check to pick up unmatched messages.
-- Needs the pid of the listen process, and node type
catchAllMsgs :: ProcessId -> String -> Process ()
catchAllMsgs pid node = buildLogMsg (node ++ " did not match a message") >>=
  Control.Distributed.Process.send pid

-- |CatchAllMsgs with MVar
catchAllMsgs' :: MVar ProcessId -> String -> Process ()
catchAllMsgs' pid node = liftIO (readMVar pid) >>=
  (`catchAllMsgs` node)
