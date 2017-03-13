{-# LANGUAGE OverloadedStrings
, ScopedTypeVariables
#-}
module Raven.DataBase
  ( ensureUsers
  , checkUser
  , getAllUsers
  , Raven.DataBase.addUser
  , deleteUser
  , updateUsersAccess
  , updateUsersPassword
  , updateUsersPassword'
  )where

import Database.MongoDB
import Crypto.Hash

import Data.Text (Text)
import qualified Data.Text as Text
import Data.List (foldl')

-- |Default root info
root :: Document
root = [ "username" := String "root"
       , "password" :=
         String (Text.pack (show (hashlazy "entry" :: Digest SHA3_512)))
       , "rootAccess" := Bool True
       ]

-- |Makes sure there is a root user.
-- If there isn't, it is created
ensureUsers :: Pipe -> IO ()
ensureUsers p = access p master "raven"
  (findOne (select ["username" := String "root"] "users") >>=
  (\u -> case u of
      Nothing -> insert_ "users" root
      _ -> return ()))

-- |Checks if a user exists, return rootAccess and id if so.
-- Returns Nothing if user not fount, Just Right if an error occurs
checkUser :: Pipe -> Text -> Text -> IO (Maybe (Either (Text,Bool) String))
checkUser p name pass = access p master "raven"
  (findOne (select ["username" := String name,"password" := String pass]
            "users") >>=
    (\doc -> case doc of
        Just doc' -> case (doc' !? "_id",doc' !? "rootAccess") of
          (Just (Oid _ id'),Just acc) -> return $ Just $ Left (Text.pack (show id'),acc)
          _ -> return $ Just $ Right $
            "User data corrupted for " ++ Text.unpack name
            ++ ", Data: " ++ show doc
        _ -> return Nothing))

-- |Get all user information (formatted)
getAllUsers :: Pipe -> IO String
getAllUsers p = access p master "raven"
  (find (select [] "users") >>= rest >>=
  return . foldl' handleUser "" >>=
  return . Text.unpack)
  where
    handleUser acc vl =
      case (vl !? "username",vl !? "rootAccess") of
        (Just name,Just (rootAcc :: Bool)) ->
          Text.concat [ name
                      , ": "
                      , Text.pack $ show rootAcc
                      , "\n"
                      , acc
                      ]
        _ -> Text.concat [ "User data corrupted\n",acc]

-- |Add a user to the database and returns outcome
addUser :: Pipe -> Text -> Text -> Bool -> IO String
addUser p name pswd rootAcc = access p master "raven"
  (find (select ["username" := String name] "users") >>= rest >>=
   (\us -> if null us
     then insert "users" [ "username" := String name
                         , "password" := String pswd
                         , "rootAccess" := Bool rootAcc
                         ] >>
          return "User created"
     else return "User already exists"))

-- |Delete a user from the database and return the id if successful
deleteUser :: Pipe -> Text -> IO (Maybe Text)
deleteUser p name = access p master "raven"
  (findOne (select ["username" := String name] "users") >>=
    (\user -> case user of
        Just user' -> delete (select ["username" := String name] "users") >>
          case user' !? "_id" of
            Just (Oid _ id') -> return $ Just $ Text.pack $ show id'
            _ -> return Nothing
        _ -> return Nothing))

-- |Change a user's root access and return the id if successful
updateUsersAccess :: Pipe -> Text -> Bool -> IO (Maybe Text)
updateUsersAccess p name rAcc = access p master "raven"
  (findOne (select ["username" := String name] "users") >>=
    (\user -> case user of
        Just user' ->
          case (user' !? "_id",user' !? "password") of
            (Just i@(Oid _ id'),Just pswd) ->
              replace (select [ "_id" := ObjId i] "users")
              [ "_id" := ObjId i
              , "username" := String name
              , "password" := String pswd
              , "rootAccess" := Bool rAcc
              ] >>
              return (Just (Text.pack (show id')))
            _ -> return Nothing
        _ -> return Nothing))

-- |Change a user's password and return Just if successful (use username)
updateUsersPassword :: Pipe -> Text -> Text -> IO (Maybe ())
updateUsersPassword p name pswd = access p master "raven"
  (findOne (select ["username" := String name] "users") >>=
    (\user -> case user of
        Just user' ->
          case (user' !? "_id",user' !? "rootAccess") of
            (Just id',Just rAcc) ->
              replace (select [ "_id" := ObjId id'] "users")
              [ "_id" := ObjId id'
              , "username" := String name
              , "password" := String pswd
              , "rootAccess" := Bool rAcc
              ] >>
              return (Just ())
            _ -> return Nothing
        _ -> return Nothing))
