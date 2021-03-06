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
import Data.Aeson.TH
import Data.Bson.Generic
import Data.Time
import Data.Char
import Data.Bits
import GHC.Generics

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
} deriving (Eq, Show, Generic, ToJSON, FromJSON, FromBSON, ToBSON)

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
--  Client
------------------------------
data Client = Client {
    username :: String,
    password :: String
} deriving (Eq, Show, Generic, ToJSON, FromJSON, FromBSON, ToBSON)

data EncryptedClient = EncryptedClient {
    unecryptedUsername :: String,
    encryptedData :: String
} deriving (Eq, Show, Generic, ToJSON, FromJSON, FromBSON, ToBSON)

data ClientRequest = ClientRequest {
    requestEncryptedClient :: EncryptedClient,
    request :: String
} deriving (Eq, Show, Generic, ToJSON, FromJSON, FromBSON, ToBSON)

data ClientFileRequest = ClientFileRequest {
    fileRequestEncryptedClient :: EncryptedClient,
    fileRequestEncryptedFile :: File
} deriving (Eq, Show, Generic, ToJSON, FromJSON, FromBSON, ToBSON)

------------------------------
--  Security 
------------------------------
data Ticket = Ticket {
    encryptedTicketId :: String,
    encryptedTimeout :: String
} deriving (Eq, Show, Generic, ToJSON, FromJSON, FromBSON, ToBSON)

data Session = Session {
    encryptedTicket :: String,
    encryptedSessionKey :: String,
    encryptedTicketTimeout :: String
} deriving (Eq, Show, Generic, ToJSON, FromJSON, FromBSON, ToBSON)

data SessionString = SessionString {
    sessionStringTicket :: Ticket,
    encryptedString :: String
} deriving (Eq, Show, Generic, FromJSON, ToJSON)

data SessionFile = SessionFile {
    sessionFileTicket :: Ticket, 
    sessionEncryptedFile :: File
} deriving (Eq, Show, Generic, FromJSON, ToJSON)

getTicketFromSession :: Session -> Ticket
getTicketFromSession (Session t k time) = Ticket t time

getSessionKeyFromTicket :: Ticket -> String
getSessionKeyFromTicket (Ticket t _) = encryptDecrypt sharedSecret t

getSessionTimeoutFromTicket :: Ticket -> UTCTime
getSessionTimeoutFromTicket (Ticket _ o) = decryptTime sharedSecret o

------------------------------
--  Responses
------------------------------
data Response = Response { 
    responseCode :: ResponseCode, 
    serverId :: Identity,
    payload :: String
} deriving (Generic, ToJSON, FromJSON, ToBSON, FromBSON)

data ResponseCode = 
    FileUploadComplete |
    FileUploadError |
    FileLockedOrReadOnly |
    DirectoryJoinSuccess |
    DirectoryError |
    SecurityClientRegistered |
    SecurityClientNotRegistered |
    SecurityError |
    SecurityClientLoggedIn
    deriving(Eq, Show, Generic, Read, ToJSON, FromJSON, ToBSON, FromBSON)

------------------------------
--  Common Variables 
------------------------------

fileServerIdentity0 :: Identity
fileServerIdentity0 = Identity "127.0.0.1" "8082" FileServer

fileServerIdentity1 :: Identity
fileServerIdentity1 = Identity "127.0.0.1" "8083" FileServer

fileServerIdentity2 :: Identity
fileServerIdentity2 = Identity "127.0.0.1" "8084" FileServer

directoryServerIdentity :: Identity
directoryServerIdentity = Identity "127.0.0.1" "8081" DirectoryServer

securityServerIdentity :: Identity
securityServerIdentity = Identity "127.0.0.1" "8080" SecurityServer

proxyServerIdentity :: Identity
proxyServerIdentity = Identity "127.0.0.1" "8079" ProxyServer

transactionServerIdentity :: Identity
transactionServerIdentity = Identity "127.0.0.1" "8078" TransactionServer

------------------------------
--  Common Functions 
------------------------------

getIdentityPort :: Identity -> Int
getIdentityPort i = read (port i):: Int

getIdentityString :: Identity -> String
getIdentityString i = address i ++ ":" ++ port i

getIdentitySafeString :: Identity -> String
getIdentitySafeString i = address i ++ "_" ++ port i

getIdentityTypeString :: Identity -> String
getIdentityTypeString i = show (serverType i) ++ "_" ++ port i

isNotValidTicket :: Ticket -> IO Bool
isNotValidTicket t = do
    let decrypytedTimeout = getSessionTimeoutFromTicket t
    currentTime <- getCurrentTime
    if currentTime > decrypytedTimeout then
        return True
    else return False

------------------------------
--  Logging Functions 
------------------------------

logHeading :: String -> IO()
logHeading s = do
    let t = "======================"
    putStrLn t
    putStrLn s
    putStrLn t

logTrailing :: IO ()
logTrailing = putStrLn "======================"

logAction :: String -> String -> String -> IO()
logAction s a m = putStrLn $ "[" ++ s ++ "]" ++ "[" ++ a ++ "]: " ++ m

logError :: String -> String -> IO()
logError s m =  putStrLn $ "[" ++ s ++ "]" ++ "[Error]: " ++ m

logDatabase :: String -> String -> String -> String -> IO()
logDatabase s d a m = putStrLn $ "[" ++ s ++ "]" ++ "[" ++ d ++ ":" ++ a ++ "]: " ++ m

logConnection :: String -> String -> String -> IO()
logConnection c s m = putStrLn $ "[" ++ c ++ "==>>" ++ s ++ "]:" ++ m

------------------------------
--  Encryption Functions 
------------------------------

sharedSecret :: String
sharedSecret = "Things we all should Know!"

encryptDecrypt :: String -> String -> String
encryptDecrypt key = zipWith (\a b -> chr $ xor (ord a) (ord b)) (cycle key)

encryptTime :: String  -> UTCTime  -> String
encryptTime key time = encryptDecrypt key (show time :: String)

decryptTime :: String  -> String  -> UTCTime
decryptTime key text = (read $ encryptDecrypt key text) :: UTCTime

encryptPort :: String  -> Int  -> String
encryptPort key port = encryptDecrypt key (show port :: String)

decryptPort :: String  -> String  -> Int
decryptPort key text = (read $ encryptDecrypt key text) :: Int

encryptDecryptArray :: String -> [String] -> [String]
encryptDecryptArray key = map (encryptDecrypt key)