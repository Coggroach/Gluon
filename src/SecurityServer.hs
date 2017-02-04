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
    run (getIdentityport securityServerIdentity) securityApp

------------------------------
--  Helper Functions
------------------------------

findClient :: String -> IO Maybe Client
findClient s = do
    logDatabase "SecurityServer" "ClientDb" "Find" s
    client <- connectToDatabase $ do
        docs <- Database.MongoDB.find (Database.MongoDB.select ["_id" =: s] "ClientDb") >>= drainCursor
        return $ Data.Maybe.mapMaybe (\ b -> fromBSON b :: Maybe Client) docs
    if null client then        
        return Nothing
    else
        return $ head client

upsertClient :: CommonServer.Client -> IO ()
upsertClient c = liftIO $ do
    logDatabase "SecurityServer" "ClientDb" "Upsert" (username c)
    connectToDatabase $ Database.MongoDB.upsert (Database.MongoDB.select ["_id" =: username c] "ClientDb") $ toBSON c

generateSessionKey :: IO String
generateSessionKey = fmap (take 12 . randomRs ('!', 'z')) newStdGen

getTimeoutTime :: Int
getTimeoutTime = next (1500, 2100)

generateSession :: String -> IO CommonServer.Session
generateSession p = do
    sessionKey <- generateSessionKey
    let encryptedSessionKey = encryptDecrypt p sessionKey
    let encryptedTicket = encryptDecrypt sharedSecret
    currentTime <- getCurrentTime
    let encryptedTicketTimeout = addUTCTime getTimeoutTime currentTime
    return (Session encryptedTicket encryptedSessionKey encryptedTicketTimeout)

getFailedSession :: String -> IO CommonServer.Session
getFailedSession s = Session "Failed" s "Failed"

------------------------------
--  Serving Functions
------------------------------
login :: CommonServer.EncryptedClient -> ApiHandler CommonServer.Session
login (CommonServer.EncryptedClient name encryptedData) = liftIO $ do
    logConnection "" "SecurityServer" "POST login"
    client <- findClient name
    case client of
        Nothing -> do
            logError "SecurityServer" "Client not Found."
            return getFailedSession "Client not Found"
        Just c -> do
            let decrypted = encryptDecrypt (password c) encryptedData
            if decrypted != name then do
                logError "SecurityServer" "Login Failed"
                logTrailing
                return getFailedSession "Login has Incorrect Encryption Data"
            else do
                logAction "SecurityServer" "Login" "Session Created"
                logTrailing
                return generateSession (password c)

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
    case client of 
        Nothing -> return (CommonServer.Response CommonServer.SecurityClientNotRegistered securityServerIdentity "")
        Just c -> return (CommonServer.Response CommonServer.SecurityClientRegistered securityServerIdentity "")
            


