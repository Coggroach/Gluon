{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ProxyServer where

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
resources = Resources "res/ProxyServer";

------------------------------
--  Server Functions
------------------------------
proxyServer :: Server ProxyApi
proxyServer =
    ProxyServer.loginClient :<|>
    ProxyServer.getFiles :<|>
    ProxyServer.openFile :<|>
    ProxyServer.closeFile -- :<|>
    --beginTransaction :<|>
    --endTransaction

proxyApp :: Application
proxyApp = serve proxyApi proxyServer

mkProxyServer :: IO()
mkProxyServer = do
    createDirectoryIfMissing True (path resources)
    setCurrentDirectory (path resources)
    logHeading "ProxyServer"
    logAction "ProxyServer" "Start" $ show (getIdentityString proxyServerIdentity)
    deleteDatabases
    run (getIdentityPort proxyServerIdentity) proxyApp

------------------------------
--  Helper Functions
------------------------------
containsFromSecurityServer :: String -> IO CommonServer.Response
containsFromSecurityServer s = do
    logConnection "ProxyServer" "SecurityServer" "POST contains"
    manager <- newManager defaultManagerSettings
    response <- runClientM (securityClientContains s) (ClientEnv manager (BaseUrl Http (address securityServerIdentity) (read(port securityServerIdentity)::Int) ""))
    case response of
        Left err -> return (CommonServer.Response CommonServer.SecurityError proxyServerIdentity "")
        Right response' -> return response'

loginToSecurityServer :: CommonServer.EncryptedClient -> IO ()
loginToSecurityServer ec = do
    logConnection "ProxyServer" "SecurityServer" "POST login"
    manager <- newManager defaultManagerSettings
    response <- runClientM (securityClientLogin ec) (ClientEnv manager (BaseUrl Http (address securityServerIdentity) (read(port securityServerIdentity)::Int) ""))
    case response of
        Left err -> logError "ProxyServer" "Problem connecting to SecurityServer"        
        Right response' -> upsertClientSession (unecryptedUsername ec) response'

registerToSecurityServer :: CommonServer.Client -> IO ()
registerToSecurityServer c = do
    logConnection "ProxyServer" "SecurityServer" "POST register"
    manager <- newManager defaultManagerSettings
    response <- runClientM (securityClientRegister c) (ClientEnv manager (BaseUrl Http (address securityServerIdentity) (read(port securityServerIdentity)::Int) ""))
    case response of
        Left err -> logError "ProxyServer" "Problem connecting to SecurityServer"        
        Right response' -> logAction "ProxyServer" "Login" "Registered to SecurityServer"

findClientSession :: String -> IO CommonServer.Session
findClientSession name = liftIO $ do
    logDatabase "ProxyServer" "ClientSessionDb" "Find" name
    connectToDatabase $ do
        docs <- Database.MongoDB.find (select ["_id" =: name] "ClientSessionDb") >>= drainCursor
        return $ head $ Data.Maybe.mapMaybe (\ b -> fromBSON b :: Maybe CommonServer.Session) docs

upsertClientSession :: String -> CommonServer.Session -> IO ()
upsertClientSession name session = liftIO $ do
    logDatabase "ProxyServer" "ClientSessionDb" "Upsert" name
    connectToDatabase $ upsert (select ["_id" =: name] "ClientSessionDb") $ toBSON session

deleteDatabases :: IO ()
deleteDatabases = liftIO $ do
    logAction "ProxyServer" "Delete" "ClientSessionDb"
    connectToDatabase $ Database.MongoDB.delete (select [] "ClientSessionDb")

createEncryptedUserNameWithPassword :: String -> String -> String
createEncryptedUserNameWithPassword u p = encryptDecrypt p u

getFilesFromDirectoryServer :: CommonServer.Ticket -> IO [FilePath]
getFilesFromDirectoryServer t = do
    logConnection "ProxyServer" "DirectoryServer" "POST files"
    manager <- newManager defaultManagerSettings
    response <- runClientM (directoryClientFiles t) (ClientEnv manager (BaseUrl Http (address directoryServerIdentity) (getIdentityPort directoryServerIdentity) ""))
    case response of
        Left err -> do
            logError "ProxyServer" "Problem connecting to DirectoryServer"
            return []
        Right response' -> do
            logAction "ProxyServer" "Fetch" "Got FileList from DirectoryServer"
            return response'

getFileFromDirectoryServer :: CommonServer.Ticket -> String -> IO CommonServer.File
getFileFromDirectoryServer t fn = do
    logConnection "ProxyServer" "DirectoryServer" "POST open"
    manager <- newManager defaultManagerSettings
    response <- runClientM (directoryClientOpen t fn) (ClientEnv manager (BaseUrl Http (address directoryServerIdentity) (getIdentityPort directoryServerIdentity) ""))
    case response of
        Left err -> do
            logError "ProxyServer" "Problem connecting to DirectoryServer"
            return (CommonServer.File "Error" "")
        Right response' -> do
            logAction "ProxyServer" "Fetch" "Got File from DirectoryServer"
            return response'

uploadFileToDirectoryServer :: CommonServer.Ticket -> CommonServer.File -> IO CommonServer.Response
uploadFileToDirectoryServer t f = do
    logConnection "ProxyServer" "DirectoryServer" "POST close"
    manager <- newManager defaultManagerSettings    
    response <- runClientM (directoryClientClose t f) (ClientEnv manager (BaseUrl Http (address directoryServerIdentity) (getIdentityPort directoryServerIdentity) ""))
    case response of
        Left err -> do
            logError "ProxyServer" "Problem connecting to DirectoryServer"
            return (CommonServer.Response CommonServer.DirectoryError proxyServerIdentity "")
        Right response' -> do
            logAction "ProxyServer" "Upload" "Uploaded File to DirectoryServer"
            return response'

------------------------------
--  Serving Functions
------------------------------
loginClient :: CommonServer.ClientRequest -> ApiHandler CommonServer.Response
loginClient (CommonServer.ClientRequest ec req) = liftIO $ do
    logConnection "" "ProxyServer" "POST login"
    let clientName = unecryptedUsername ec
    let clientPassword = encryptedData ec
    response <- containsFromSecurityServer clientName
    let doesSecurityContain = responseCode response
    case doesSecurityContain of
        CommonServer.SecurityError -> do
            logError "ProxyServer" "Problem connecting to SecurityServer"
            logTrailing
            return response
        CommonServer.SecurityClientRegistered -> do
            loginToSecurityServer ec
            logTrailing
            return (CommonServer.Response CommonServer.SecurityClientLoggedIn securityServerIdentity "Client Logged In")
        CommonServer.SecurityClientNotRegistered -> do
            registerToSecurityServer (CommonServer.Client clientName clientPassword)
            loginToSecurityServer (CommonServer.EncryptedClient clientName (createEncryptedUserNameWithPassword clientName clientPassword))
            logTrailing
            return (CommonServer.Response CommonServer.SecurityClientLoggedIn securityServerIdentity "Client Registered")

getFiles :: CommonServer.ClientRequest -> ApiHandler [FilePath]
getFiles (CommonServer.ClientRequest ec req) = liftIO $ do
    logConnection "" "ProxyServer" "POST files"
    session <- findClientSession $ unecryptedUsername ec
    let ticket = getTicketFromSession session    
    files <- getFilesFromDirectoryServer ticket
    logTrailing
    return files

openFile :: CommonServer.ClientRequest -> ApiHandler CommonServer.File
openFile (CommonServer.ClientRequest ec req) = liftIO $ do
    logConnection "" "ProxyServer" "POST open"
    session <- findClientSession $ unecryptedUsername ec
    let ticket = getTicketFromSession session    
    file <- getFileFromDirectoryServer ticket req
    logTrailing
    return file

closeFile :: CommonServer.ClientFileRequest -> ApiHandler CommonServer.Response
closeFile (CommonServer.ClientFileRequest ec req) = liftIO $ do
    logConnection "" "ProxyServer" "POST close"
    session <- findClientSession $ unecryptedUsername ec
    let ticket = getTicketFromSession session
    response <-  uploadFileToDirectoryServer ticket req
    logTrailing
    return response

--begin

--end