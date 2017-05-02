{-# LANGUAGE OverloadedStrings #-}
module Raven.Server.Commands
  ( parseCommand
  )where

import Network.Transport (ConnectionId)
import Control.Distributed.Process
import Crypto.Hash

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.List (foldl')
import Text.Read

import Raven.Server.NodeMsgs

-- |A single command record representing all information.
-- parseFunc is a function that handles the parsing and needs first
-- the servers pid then its own pid.
-- help is a function that sends information on the command, needs its own pid,
-- and the n value of the message.
data Cmd = Cmd { parseFunc :: ProcessId -> ConnectionId -> [ByteString] -> Process ()
               , help :: ByteString
               }

-- |Current command map
cmdMap :: Map ByteString Cmd
cmdMap = Map.fromList
  [ (":kill",Cmd { parseFunc = parseKill
                 , help = helpKill
                 })
  , (":logon",Cmd { parseFunc = parseLogon
                  , help = helpLogon
                  })
  , (":repl?",Cmd { parseFunc = parseReplq
                  , help = helpReplq
                  })
  , (":logout",Cmd { parseFunc = parseLogout
                   , help = helpLogout
                   })
  , (":stopRepl",Cmd { parseFunc = parseStopRepl
                     , help = helpStopRepl
                     })
  , (":allUsers",Cmd { parseFunc = parseAllUsers
                     , help = helpAllUsers
                     })
  , (":help",Cmd { parseFunc = parseHelp cmdMap
                 , help = helpHelp
                 })
  , (":addUser",Cmd { parseFunc = parseAddUser
                    , help = helpAddUser
                    })
  , (":deleteUser",Cmd { parseFunc = parseDeleteUser
                       , help = helpDeleteUser
                       })
  , (":grantAccess",Cmd { parseFunc = parseGrantAccess
                        , help = helpGrantAccess
                        })
  , (":removeAccess",Cmd { parseFunc = parseRemoveAccess
                         , help = helpRemoveAccess
                         })
  , (":changePassword",Cmd { parseFunc = parseChangePassword
                           , help = helpChangePassword
                           })
  , (":plot", Cmd { parseFunc = parsePlot
                  , help = helpPlot
                  })
  ]

-- |runs commands
parseCommand :: ProcessId -> ConnectionId -> [ByteString] -> Process ()
parseCommand server self msg@(n:cmd:_) =
  case Map.lookup cmd cmdMap of
    Just Cmd{parseFunc=f} -> f server self msg
    _ -> send server $ ProcessedMsg n "Command not found"
parseCommand server _ _ =
  buildLogMsg "Command pattern match failed" >>=
  send server

-- |Parses a kill command
parseKill :: ProcessId -> ConnectionId -> [ByteString] -> Process ()
parseKill server _ [n,":kill"] = send server $ KillMsg n
parseKill server _ (n:":kill":_) =
  send server (ProcessedMsg n ":kill takes no arguments")
parseKill server _ _ =
  buildLogMsg "Command pattern match failed" >>=
  send server

-- |sends information on a kill command
helpKill :: ByteString
helpKill =
  ":kill sends a kill command to the server, shutting it down.\n\
  \The command accepts no arguments, and requires a logged on user\
  \with root access."

-- |Parses a logon command
parseLogon :: ProcessId -> ConnectionId -> [ByteString] -> Process ()
parseLogon server self [n,":logon",name,pass] =
  send server (self,LoginMsg n  (Text.pack (B.unpack name))
                (Text.pack (show (hash pass :: Digest SHA3_512))))
parseLogon server _ (n:":logon":_) =
  send server (ProcessedMsg n ":logon takes two arguments (username password)")
parseLogon server _ _ =
  buildLogMsg "Command pattern match failed" >>=
  send server

-- |Sends information on a logon command
helpLogon :: ByteString
helpLogon =
  ":logon logs a user on to the server.\n\
  \The command accepts two arguments, username and password."

-- |Parses a repl? command
parseReplq :: ProcessId -> ConnectionId -> [ByteString] -> Process ()
parseReplq server self [n,":repl?"] = send server (self,REPLInfoMsg n)
parseReplq server _ (n:":repl?":_) =
  send server (ProcessedMsg n ":repl? takes no arguments")
parseReplq server _ _ =
  buildLogMsg "Command pattern match failed" >>=
  send server

-- |Sends information on a repl? command
helpReplq :: ByteString
helpReplq =
  ":repl? determines if the current user has a repl running.\n\
  \The command accepts no arguments and requires a logged on user."

-- |parse a logout command
parseLogout :: ProcessId -> ConnectionId -> [ByteString] -> Process ()
parseLogout server self [n,":logout"] = send server (self,LogoutMsg n)
parseLogout server _ (n:":logout":_) =
  send server (ProcessedMsg n ":logout takes no arguments")
parseLogout server _ _ =
  buildLogMsg "Command pattern match failed" >>=
  send server

-- |sends information about a logout command
helpLogout :: ByteString
helpLogout =
  ":logout logs the current user out of the sever.\n\
  \The command accepts no arguments and requires a logged on user."

-- |Parse a stop repl command
parseStopRepl :: ProcessId -> ConnectionId -> [ByteString] -> Process ()
parseStopRepl server self [n,":stoprepl"] =
  send server (self,StopREPLMSG n)
parseStopRepl server _ (n:":stopRepl":_) =
  send server (ProcessedMsg n ":stoprepl take no arguments")
parseStopRepl server _ _ =
  buildLogMsg "Command pattern match failed" >>=
  send server

-- |sends information about a stopRepl command
helpStopRepl :: ByteString
helpStopRepl =
  ":stopRepl checks if the current user has a repl running, and if so,\
  \stops it.\n\
  \The command accepts no arguments and requires a logged on user."

-- |parse the allUsers command
parseAllUsers :: ProcessId -> ConnectionId -> [ByteString] -> Process ()
parseAllUsers server self [n,":allUsers"] = send server (self,AllUsersMsg n)
parseAllUsers server _ (n:":allUsers":_) =
  send server (ProcessedMsg n ":allUsers accepts no arguments")
parseAllUsers server _ _ =
  buildLogMsg "Command pattern match failed" >>=
  send server

-- |sends information about an allUsers command
helpAllUsers :: ByteString
helpAllUsers =
  ":allUsers returns all users that exist in the server's database\n\
  \The command accepts no arguments and requires a logged on user with\
  \root access."

-- |parse the help command
parseHelp :: Map ByteString Cmd -> ProcessId -> ConnectionId -> [ByteString] -> Process ()
parseHelp cMap server _ [n,":help"] =
  send server $ ProcessedMsg n $ B.unpack $ foldl' helpConcat B.empty $
  Map.toList cMap
  where
    helpConcat acc (cmd,Cmd{help=help'}) =
      B.concat [ acc
               , cmd
               , ":\n"
               , help'
               , "\n"
               ]
parseHelp cMap server _ (n:":help":cmds) =
  send server $ ProcessedMsg n $ B.unpack $ foldl' helpConcat B.empty cmds
  where
    helpConcat acc cmd = case Map.lookup cmd cMap of
      Just Cmd{help=help'} -> B.concat [ acc
                                       , ":\n"
                                       , help'
                                       , "\n"
                                       ]
      _ -> B.concat [ acc
                    , "Command "
                    , cmd
                    , " not found\n"]
parseHelp _ server _ _ =
  buildLogMsg "Command pattern match failed" >>=
  send server

-- |sends information about the help command
helpHelp :: ByteString
helpHelp =
  ":help returns information on existing commands.\n\
  \The command either takes no arguments and returns information on all commands, \n\
  \or takes a list of commands and searches for the accompanying information."

-- |parse the addUsers command
parseAddUser :: ProcessId -> ConnectionId -> [ByteString] -> Process ()
parseAddUser server self [n,":addUser",name,pass] =
  send server (self,AddUserMsg n (Text.pack (B.unpack name))
              (Text.pack (show (hash pass :: Digest SHA3_512))) False)
parseAddUser server self [n,":addUser",name] =
  send server (self,AddUserMsg n (Text.pack (B.unpack name))
              (Text.pack (show (hashlazy "entry" :: Digest SHA3_512))) False)
parseAddUser server self [n,":addUser",name,pass,root] =
  case (readMaybe (B.unpack root) :: Maybe Bool) of
    Just root' -> send server (self,AddUserMsg n (Text.pack (B.unpack name))
                                (Text.pack (show (hash pass :: Digest SHA3_512))) root')
    _ -> send server (self,AddUserMsg n (Text.pack (B.unpack name))
                       (Text.pack (show (hash pass :: Digest SHA3_512))) False)
parseAddUser server _ (n:":addUser":_) =
  send server (ProcessedMsg n ":addUser accepts [1,3] arguments")
parseAddUser server _ _ =
  buildLogMsg "Command pattern match failed" >>=
  send server

-- |information for addUsers
helpAddUser :: ByteString
helpAddUser =
  ":addUser attempts to add a user and sends back the result.\n\
  \If the command is given one argument the argument is the username,\n\
  \the password is entry, and the user does not have root access.\n\
  \If the command is given two arguments the first argument is the username,\n\
  \the second is the password, and the user is not given root access.\n\
  \If the command is given three arguments the first argument is the username,\n\
  \the second is the password, and the user is given the supplied root access if\n\
  \the third argument can be parsed into a Bool, and doesn't have access otherwise.\n\
  \This command requires a logged in user with root access."

-- |parse a deleteUser command
parseDeleteUser :: ProcessId -> ConnectionId -> [ByteString] -> Process ()
parseDeleteUser server _ [n,":deleteUser","root"] =
  send server $ ProcessedMsg n "You cannot delete this user"
parseDeleteUser server self [n,":deleteUser",usr] =
  send server (self,DeleteUserMsg n $ Text.pack $ B.unpack usr)
parseDeleteUser server _ (n:":deleteUser":_) =
  send server $ ProcessedMsg n ":deleteUser takes one argument"
parseDeleteUser server _ _ =
  buildLogMsg "Command pattern match failed" >>=
  send server

-- |deleteUser information
helpDeleteUser :: ByteString
helpDeleteUser =
  ":deleteUser attempts to remove a user from the server and sends back the result.\n\
  \The command takes one argument (the username) and requires a logged on user with \
  \root access."

-- |parse a grantAccess command
parseGrantAccess :: ProcessId -> ConnectionId -> [ByteString] -> Process ()
parseGrantAccess server _ [n,":grantAccess","root"] =
  send server $ ProcessedMsg n "root has access by default"
parseGrantAccess server self [n,":grantAccess",usr] =
  send server (self,ChangeRootAccessMsg n (Text.pack (B.unpack usr)) True)
parseGrantAccess server _ (n:":grantAccess":_) =
  send server $ ProcessedMsg n ":grantAccess takes one argument"
parseGrantAccess server _ _ =
  buildLogMsg "Command pattern match failed" >>=
  send server

-- |grantAccess information
helpGrantAccess :: ByteString
helpGrantAccess =
  ":grantAccess attempts to give a user root access and sends back the result.\n\
  \The command takes one argument (the username) and requires a logged on user with \
  \root access."

-- |parse a removeAccess command
parseRemoveAccess :: ProcessId -> ConnectionId -> [ByteString] -> Process ()
parseRemoveAccess server _ [n,":removeAccess","root"] =
  send server $ ProcessedMsg n "root cannot loose root access"
parseRemoveAccess server self [n,":removeAccess",usr] =
  send server (self,ChangeRootAccessMsg n (Text.pack (B.unpack usr)) False)
parseRemoveAccess server _ (n:":removeAccess":_) =
  send server $ ProcessedMsg n ":removeAccess takes one argument"
parseRemoveAccess server _ _ =
  buildLogMsg "Command pattern match failed" >>=
  send server

-- |grantAccess information
helpRemoveAccess :: ByteString
helpRemoveAccess =
  ":removeAccess attempts to give a user root access and sends back the result.\n\
  \The command takes one argument (the username) and requires a logged on user with \
  \root access."

-- |parse a changePassword command
parseChangePassword :: ProcessId -> ConnectionId -> [ByteString] -> Process ()
parseChangePassword server self [n,":changePassword",name,new] =
  send server (self,ChangeUsersPasswordMsg n
                    (Text.pack (B.unpack name))
                    (Text.pack (show (hash new :: Digest SHA3_512))))
parseChangePassword server _ (n:":changePassword":_) =
  send server $ ProcessedMsg n ":changePassword takes two arguments"
parseChangePassword server _ _ =
  buildLogMsg "Command pattern match failed" >>=
  send server

-- |changePassword information
helpChangePassword :: ByteString
helpChangePassword =
  ":changePassword attempts to change a user's password and sends back the result.\n\
  \The command takes two arguments, \n\
  \the first is the username, the second the new password,\
  \Changing a user's password requires a logged on user with root access."

-- |parse a plot command
parsePlot :: ProcessId -> ConnectionId -> [ByteString] -> Process ()
parsePlot server _ [n,":plot",_] = send server $
  ProcessedMsg n ":plot takes two arguments"
parsePlot server self bs@(n:":plot":ptype:_) = send server
  (self, PlotMsg n (B.unpack ptype) (B.unpack (B.unwords (drop 3 bs))))
parsePlot server _ (n:":plot":_) = send server $
  ProcessedMsg n ":plot takes two arguments"
parsePlot server _ _ =
  buildLogMsg "Command pattern match failed" >>=
  send server

-- |plot information
helpPlot :: ByteString
helpPlot =
  ":plot attempts to build a graph and send back the image to the client.\n\
  \The command takes two arguments, the first is the plot type,\n\
  \the second the points (in a statement which is interpreted)"
