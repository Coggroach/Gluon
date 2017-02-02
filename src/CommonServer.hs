{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveAnyClass#-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CommonServer where
import Data.Aeson
import Data.Bson.Generic
import GHC.Generics
import System.IO.Unsafe

deriving instance FromBSON String
deriving instance ToBSON   String

deriving instance FromBSON Bool
deriving instance ToBSON Bool

------------------------------
--  File Structure
------------------------------
data File = File { 
    fileName :: FilePath, 
    fileContent :: String 
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

------------------------------
--  Server Identity 
------------------------------
data Identity = Identity { 
    address :: String, 
    port :: String,
    serverType :: ServerType
} deriving (Generic, Eq, Show, ToJSON, FromJSON, FromBSON, ToBSON)

------------------------------
--  Registered Server Types 
------------------------------
data ServerType = 
    FileServer |
    DirectoryServer |
    ProxyServer |
    SecurityServer |
    TransactionServer |
    ReplicationServer
    deriving(Eq, Show, Generic, Read, ToJSON, FromJSON, FromBSON, ToBSON)


------------------------------
--  Resources Directory 
------------------------------
data Resources = Resources { 
    path :: String     
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

------------------------------
--  Client Data
------------------------------
data Client = Client {
    username :: String,
    password :: String
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

------------------------------
--  Security Token
------------------------------
data Token = Token {
    sessionId :: String,
    sessionKey :: String,
    ticket :: String,
    client :: Identity
} deriving (Generic, ToJSON, FromJSON)

------------------------------
--  Response Packet 
------------------------------
data Response = Response { 
    responseCode :: ResponseCode, 
    serverId :: Identity,
    payload :: String
} deriving (Generic, ToJSON, FromJSON, ToBSON, FromBSON)

------------------------------
--  Response Codes 
------------------------------
data ResponseCode = 
    FileUploadComplete |
    FileUploadError |
    DirectoryJoinSuccess
    deriving(Eq, Show, Generic, Read, ToJSON, FromJSON, ToBSON, FromBSON)

------------------------------
--  Common Variables 
------------------------------

fileServerIdentity :: Identity
fileServerIdentity = Identity "localhost" "8082" FileServer

directoryServerIdentity :: Identity
directoryServerIdentity = Identity "localhost" "8083" DirectoryServer

------------------------------
--  Common Functions 
------------------------------

getIdentityPort :: Identity -> Int
getIdentityPort i = read (port i):: Int