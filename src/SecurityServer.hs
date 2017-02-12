{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module SecurityServer where

import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Bson.Generic
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API
import           Servant.Client
import           System.IO
import           System.Directory
import           CommonServer
import           CommonServerApi
import           Data.Map
import           Data.Time
import           Data.List
import           Data.Maybe                   (catMaybes, mapMaybe)
import           Data.Text                    (pack, unpack)
import           System.Random
import           Network.HTTP.Client (newManager, defaultManagerSettings)
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           Database.MongoDB
import           MongoDbConnector

------------------------------
--  Variables
------------------------------
resources :: Resources
resources = Resources "res/SecurityServer";

------------------------------
--  Server Functions
------------------------------
securityServer :: Server SecurityApi
securityServer =
    login :<|>
    register :<|>
    contains

securityApp :: Application
securityApp = serve securityApi securityServer

mkSecurityServer :: IO()
mkSecurityServer = do
    createDirectoryIfMissing True (path resources)
    setCurrentDirectory (path resources)
    logHeading "SecurityServer"
    logAction "SecurityServer" "Start" $ show (getIdentityString securityServerIdentity)
    run (getIdentityPort securityServerIdentity) securityApp

------------------------------
--  Helper Functions
------------------------------
findClient :: String -> IO [CommonServer.Client]
findClient s = liftIO $ do
    logDatabase "SecurityServer" "ClientDb" "Find" s
    connectToDatabase $ do
        docs <- Database.MongoDB.find (Database.MongoDB.select ["_id" =: s] "ClientDb") >>= drainCursor
        return $ Data.Maybe.mapMaybe (\ b -> fromBSON b :: Maybe CommonServer.Client) docs

upsertClient :: CommonServer.Client -> IO ()
upsertClient c = liftIO $ do
    logDatabase "SecurityServer" "ClientDb" "Upsert" (username c)
    connectToDatabase $ Database.MongoDB.upsert (Database.MongoDB.select ["_id" =: username c] "ClientDb") $ toBSON c

generateSessionKey :: IO String
generateSessionKey = fmap (take 12 . randomRs ('!', 'z')) newStdGen

generateSession :: String -> IO CommonServer.Session
generateSession p = do
    sessionKey <- generateSessionKey
    let encryptedSessionKey = encryptDecrypt p sessionKey
    let encryptedTicket = encryptDecrypt sharedSecret sessionKey
    currentTime <- getCurrentTime
    let encryptedTicketTimeout = encryptTime sharedSecret $ addUTCTime 1800 currentTime
    return (CommonServer.Session encryptedTicket encryptedSessionKey encryptedTicketTimeout)

getFailedSession :: String -> CommonServer.Session
getFailedSession s = CommonServer.Session "Failed" s "Failed"

------------------------------
--  Serving Functions
------------------------------
login :: CommonServer.EncryptedClient -> ApiHandler CommonServer.Session
login (CommonServer.EncryptedClient name encryptedData) = liftIO $ do
    logConnection "" "SecurityServer" "POST login"
    client <- findClient name
    case length client of
        0 -> do
            logError "SecurityServer" "Client not Found."
            return $ getFailedSession "Client not Found"
        _ -> do
            let c = head client
            let decrypted = encryptDecrypt (password c) encryptedData
            if decrypted == name then do
                logAction "SecurityServer" "Login" "Session Created"
                logTrailing
                generateSession (password c)
            else do
                logError "SecurityServer" "Login Failed"
                logTrailing
                return $ getFailedSession "Login has Incorrect Encryption Data"

register :: CommonServer.Client -> ApiHandler CommonServer.Response
register c = liftIO $ do
    logConnection "" "SecurityServer" "POST register"
    upsertClient c
    logTrailing
    return (CommonServer.Response CommonServer.SecurityClientRegistered securityServerIdentity "")

contains :: String -> ApiHandler CommonServer.Response
contains c = liftIO $ do
    logConnection "" "securityServer" "GET contains"
    client <- findClient c
    logTrailing
    case length client of 
        0 -> return (CommonServer.Response CommonServer.SecurityClientNotRegistered securityServerIdentity "")
        _ -> return (CommonServer.Response CommonServer.SecurityClientRegistered securityServerIdentity "")
            


