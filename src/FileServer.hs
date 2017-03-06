{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module FileServer where

import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class
import           Data.Aeson
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
import           Network.HTTP.Client (newManager, defaultManagerSettings)

------------------------------
--  Variables
------------------------------
resources :: Resources
resources = Resources "res/FileServers";

fileServerIdentity :: Identity
fileServerIdentity = fileServerIdentity0

------------------------------
--  Server Functions
------------------------------
fileServer :: Server FileApi
fileServer = 
    getFiles :<|>
    downloadFile :<|>
    uploadFile

fileApp :: Application
fileApp = serve fileApi fileServer

mkFileServer :: Identity -> IO()
mkFileServer i = do
    let dir = path resources
    createDirectoryIfMissing True dir
    setCurrentDirectory dir
    let fileServerIdentity = Identity (address i) (port i) (serverType i)
    logHeading "FileServer"    
    logConnection "FileServer" "DirectoryServer" "POST join"
    manager <- newManager defaultManagerSettings
    response <- runClientM (directoryClientJoin fileServerIdentity) (ClientEnv manager (BaseUrl Http (address directoryServerIdentity) (getIdentityPort directoryServerIdentity) ""))    
    case response of
        Left err -> logError "FileServer" $ show err
        Right response -> logAction "FileServer" "Done" ""
    logAction "FileServer" "Start" $ show (getIdentityString fileServerIdentity)
    run (getIdentityPort fileServerIdentity) fileApp 

------------------------------
--  Serving Functions
------------------------------
getFiles :: CommonServer.Ticket -> ApiHandler [FilePath]
getFiles t = liftIO $ do
    logConnection "" "FileServer" "POST files"
    dir <- getCurrentDirectory
    logTrailing
    listDirectory dir

downloadFile :: CommonServer.Ticket -> String -> ApiHandler File
downloadFile t fn = liftIO $ do
    flag <- isNotValidTicket t
    if flag then do
        logError "FileServer" "Session Timed out"
        return (CommonServer.File fn (encryptDecrypt (getSessionKeyFromTicket t) "Timeout")) 
    else do
        logConnection "" "FileServer" "POST download"
        let decryptedFileName = encryptDecrypt (getSessionKeyFromTicket t) fn
        content <- liftIO (readFile decryptedFileName)
        logTrailing
        return (File fn content)

uploadFile :: CommonServer.Ticket -> File -> ApiHandler CommonServer.Response
uploadFile t (File fn c) = liftIO $ do
    flag <- isNotValidTicket t
    if flag then do
        logError "FileServer" "Session Timed out"
        return (CommonServer.Response CommonServer.FileUploadError fileServerIdentity "")
    else do
        logConnection "" "FileServer" "POST upload"
        let decryptedFileName = encryptDecrypt (getSessionKeyFromTicket t) fn
        let decryptedFileContent = encryptDecrypt (getSessionKeyFromTicket t) c
        liftIO (writeFile decryptedFileName decryptedFileContent)
        logTrailing
        return (CommonServer.Response CommonServer.FileUploadComplete fileServerIdentity "")