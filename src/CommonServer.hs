{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module CommonServer where
import Data.Aeson
import Data.Bson.Generic
import GHC.Generics
import Network.Info
import System.IO.Unsafe
------------------------------
--  File Structure
------------------------------
data File = File { 
    fileName :: FilePath, 
    fileContent :: String 
} deriving (Eq, Show, Generic)

instance ToJSON File
instance FromJSON File

------------------------------
--  Server Identity 
------------------------------
data Identity = Identity { 
    address :: String, 
    port :: String,
    serverType :: ServerType
} deriving (Generic)

instance ToJSON Identity
instance FromJSON Identity

deriving instance FromBSON Identity
deriving instance ToBSON   Identity

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
    deriving(Eq, Show, Generic, Read)
instance ToJSON ServerType
instance FromJSON ServerType

------------------------------
--  Resources Directory 
------------------------------
data Resources = Resources { 
    path :: String     
} deriving (Eq, Show, Generic)

instance ToJSON Resources
instance FromJSON Resources

------------------------------
--  Client Data
------------------------------
data Client = Client {
    username :: String,
    password :: String
} deriving (Eq, Show, Generic)
instance ToJSON Client
instance FromJSON Client
------------------------------
--  Security Token
------------------------------
data Token = Token {
    sessionId :: String,
    sessionKey :: String,
    ticket :: String,
    client :: Identity
} deriving (Generic)
instance ToJSON Token
instance FromJSON Token

------------------------------
--  Response Packet 
------------------------------
data Response = Response { 
    responseCode :: ResponseCode, 
    serverId :: Identity,
    payload :: String
} deriving (Generic)

instance ToJSON Response
instance FromJSON Response

------------------------------
--  Response Codes 
------------------------------
data ResponseCode = 
    FileUploadComplete |
    FileUploadError
    deriving(Eq, Show, Generic, Read)
instance ToJSON ResponseCode
instance FromJSON ResponseCode

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