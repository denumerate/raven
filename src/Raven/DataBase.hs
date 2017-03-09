{-# LANGUAGE OverloadedStrings
, ScopedTypeVariables
#-}
module Raven.DataBase
  ( ensureUsers
  , checkUser
  , getAllUsers
  , Raven.DataBase.addUser
  )where

import Database.MongoDB
import Crypto.Hash

import Data.Text (Text)
import qualified Data.Text as Text
import Data.ByteString.Char8 (ByteString)
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
addUser :: Pipe -> Text -> ByteString -> Bool -> IO String
addUser p name pswd rootAcc = access p master "raven"
  (find (select ["username" := String name] "users") >>= rest >>=
   (\us -> if null us
     then insert "users" [ "username" := String name
                         , "password" :=
                           String (Text.pack (show (hash pswd :: Digest SHA3_512)))
                         , "rootAccess" := Bool rootAcc
                         ] >>
          return "User created"
     else return "User already exists"))
