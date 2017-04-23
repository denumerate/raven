{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module Raven.Server.NodeMsgs
  ( REPLMsg(..)
  , ProcessedMsg(..)
  , KillMsg(..)
  , LogMsg(..)
  , LoginMsg(..)
  , LoginSucMsg(..)
  , NewTokenMsg(..)
  , LogoutMsg(..)
  , REPLInfoMsg(..)
  , StopREPLMSG(..)
  , AllUsersMsg(..)
  , AddUserMsg(..)
  , DeleteUserMsg(..)
  , DeleteUserSuccMsg(..)
  , ChangeRootAccessMsg(..)
  , RootAccessChangedMsg(..)
  , ChangeUsersPasswordMsg(..)
  , PlotMsg(..)
  , PlotDoneMsg(..)
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
                  | ProcessedBSMsg ByteString ByteString
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

-- |NewTnameokenMsg holds: (Token, id)
data NewTokenMsg = NewTokenMsg (Text,Text)
  deriving (Generic,Binary,Typeable)

data LogoutMsg = LogoutMsg ByteString
  deriving (Generic,Binary,Typeable)

data REPLInfoMsg = REPLInfoMsg ByteString
  deriving (Generic,Binary,Typeable)

data StopREPLMSG = StopREPLMSG ByteString
  deriving (Generic,Binary,Typeable)

data AllUsersMsg = AllUsersMsg ByteString
  deriving (Generic,Binary,Typeable)

-- |n,username,passwd,rootaccess
data AddUserMsg = AddUserMsg ByteString Text Text Bool
  deriving (Generic,Binary,Typeable)

data DeleteUserMsg = DeleteUserMsg ByteString Text
  deriving (Generic,Binary,Typeable)

data DeleteUserSuccMsg = DeleteUserSuccMsg ByteString Text
  deriving (Generic,Binary,Typeable)

-- |n username newAccess
data ChangeRootAccessMsg = ChangeRootAccessMsg ByteString Text Bool
  deriving (Generic,Binary,Typeable)

-- |same scheme as above
data RootAccessChangedMsg = RootAccessChangedMsg ByteString Text Bool
  deriving (Generic,Binary,Typeable)

-- | n username newPassword
data ChangeUsersPasswordMsg = ChangeUsersPasswordMsg ByteString Text Text
  deriving (Generic,Binary,Typeable)

-- |n type points
data PlotMsg = PlotMsg ByteString String String
  deriving (Generic,Binary,Typeable)

-- |n filename
data PlotDoneMsg = PlotDoneMsg ByteString String
  deriving (Generic,Binary,Typeable)

-- |Standardized LogMsg
buildLogMsg :: String -> Process LogMsg
buildLogMsg msg = getSelfPid >>=
  (\self -> fmap (LogMsg msg self . show) (liftIO getCurrentTime))

-- |Final check to pick up unmatched messages.
-- Needs the pid of the listen process, and node type
catchAllMsgs :: ProcessId -> String -> Process ()
catchAllMsgs pid node = buildLogMsg (node ++ " did not match a message") >>=
  Control.Distributed.Process.send pid

-- |CatchAllMsgs with MVar
catchAllMsgs' :: MVar ProcessId -> String -> Process ()
catchAllMsgs' pid node = liftIO (readMVar pid) >>=
  (`catchAllMsgs` node)
