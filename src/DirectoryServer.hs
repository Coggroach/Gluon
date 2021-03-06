{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module DirectoryServer where

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
data FileMapping = FileMapping{
    fileName :: FilePath,
    identity :: CommonServer.Identity,
    lock :: FileLock
} deriving (Eq, Show, Generic,ToJSON, FromJSON, ToBSON, FromBSON)

data FileLock = 
    FileLocked |
    FileUnlocked |
    FileReadOnly
    deriving (Eq, Show, Generic,ToJSON, FromJSON, ToBSON, FromBSON)

resources :: Resources
resources = Resources "res/DirectoryServer";

------------------------------
--  Server Functions
------------------------------
directoryServer :: Server DirectoryApi
directoryServer =
    DirectoryServer.getFiles :<|>
    DirectoryServer.openFile :<|>
    DirectoryServer.closeFile :<|>
    DirectoryServer.joinServer

directoryApp :: Application
directoryApp = serve directoryApi directoryServer

mkDirectoryServer :: IO()
mkDirectoryServer = do
    createDirectoryIfMissing True (path resources)
    setCurrentDirectory (path resources)
    logHeading "DirectoryServer"
    logAction "DirectoryServer" "Start" $ show (getIdentityString directoryServerIdentity)
    deleteDatabases
    run (getIdentityPort directoryServerIdentity) directoryApp

------------------------------
--  Helper Functions
------------------------------
deleteDatabases :: IO ()
deleteDatabases = liftIO $ do
    logAction "DirectoryServer" "Delete" "FileMappingDb, FileServerDb"
    connectToDatabase $ do
        Database.MongoDB.delete (select [] "FileMappingDb")
        Database.MongoDB.delete (select [] "FileServerDb")

upsertFileMapping :: CommonServer.Identity -> [FileMapping] -> String -> IO [FileMapping]
upsertFileMapping id array filename = liftIO $ do
    logDatabase "DirectoryServer" "FileMappingDb" "Upsert" filename
    let filemapping = FileMapping filename id FileUnlocked
    connectToDatabase $ Database.MongoDB.upsert (Database.MongoDB.select ["_id" =: filename] "FileMappingDb") $ toBSON filemapping
    return $ FileMapping filename id FileUnlocked : array

updateFileMapping :: String -> FileLock -> IO()
updateFileMapping s fl = liftIO $ do
    fileMapping <- findFileMapping s
    let newFileMapping = FileMapping s (identity fileMapping) fl
    logDatabase "DirectoryServer" "FileMappingDb" "Upsert" (s ++ show fl)
    connectToDatabase $ Database.MongoDB.upsert (Database.MongoDB.select ["_id" =: s] "FileMappingDb") $ toBSON newFileMapping


upsertFileServer :: CommonServer.Identity -> IO()
upsertFileServer i = liftIO $ do
    let key = getIdentitySafeString i
    logDatabase "DirectoryServer" "FileServerDb" "Upsert" key
    connectToDatabase $ Database.MongoDB.upsert (Database.MongoDB.select ["_id" =: key] "FileServerDb") $ toBSON i

getFilesFromFileServer :: CommonServer.Ticket -> CommonServer.Identity -> IO()
getFilesFromFileServer t i = liftIO $ do
    logConnection "DirectoryServer" "FileServer" "GET files"
    manager <- newManager defaultManagerSettings
    response <- runClientM (fileClientFiles t) (ClientEnv manager (BaseUrl Http (address i) (read(port i)::Int) ""))
    case response of
        Left err -> logError "DirectoryServer" $ show err
        Right response' -> do
            mapM_ (upsertFileMapping i []) response'
            logAction "DirectoryServer" "Done" ""

findFileMapping :: String -> IO FileMapping
findFileMapping key = liftIO $ do
    logDatabase "DirectoryServer" "FileMappingDb" "Find" key
    filemapping <- connectToDatabase $ do
        docs <- Database.MongoDB.find (Database.MongoDB.select ["_id" =: key] "FileMappingDb") >>= drainCursor
        return $ Data.Maybe.mapMaybe (\ b -> fromBSON b :: Maybe FileMapping) docs
    return $ head filemapping

getAllFileServers :: IO [CommonServer.Identity]
getAllFileServers = liftIO $ do
    logDatabase "DirectoryServer" "FileServerDb" "Find" "ALL"
    connectToDatabase $ do
        docs <- Database.MongoDB.find (Database.MongoDB.select [] "FileServerDb") >>= drainCursor
        return $ Data.Maybe.mapMaybe (\ b -> fromBSON b :: Maybe CommonServer.Identity) docs

getAllFileMappings :: IO [FileMapping]
getAllFileMappings = liftIO $ do
    logDatabase "DirectoryServer" "FileMappingDb" "Find" "ALL"
    connectToDatabase $ do
        docs <- Database.MongoDB.find (Database.MongoDB.select [] "FileMappingDb") >>= drainCursor
        return $ Data.Maybe.mapMaybe (\ b -> fromBSON b :: Maybe FileMapping) docs

downloadFromFileServer :: CommonServer.Ticket -> String -> CommonServer.Identity -> IO CommonServer.File
downloadFromFileServer t fn i = do
    logConnection "DirectoryServer" "FileServer" "GET download"
    manager <- newManager defaultManagerSettings
    response <- runClientM (fileClientDownload t fn) (ClientEnv manager (BaseUrl Http (address i) (read(port i)::Int) ""))
    case response of
        Left err -> return (File "" "")
        Right response' -> return response'

uploadToFileServer :: CommonServer.Ticket -> CommonServer.File -> CommonServer.Identity -> IO CommonServer.Response
uploadToFileServer t f i = do
    logConnection "DirectoryServer" "FileServer" "POST upload"
    manager <- newManager defaultManagerSettings
    response <- runClientM (fileClientUpload t f) (ClientEnv manager (BaseUrl Http (address i) (read(port i)::Int) ""))
    case response of
        Left err -> return (CommonServer.Response CommonServer.FileUploadError i "")
        Right response' -> return response'

------------------------------
--  Serving Functions
------------------------------
getFiles :: CommonServer.Ticket -> ApiHandler [FilePath]
getFiles t = liftIO $ do
    flag <- isNotValidTicket t
    if flag then do
        logError "DirectoryServer" "Session Timed out"
        return (encryptDecryptArray (getSessionKeyFromTicket t) ["Timeout", "Session Expired"])
    else do
        logConnection "" "DirectoryServer" "GET files"
        fileServers <- getAllFileServers
        mapM_ (getFilesFromFileServer t) fileServers
        fileMappings <- getAllFileMappings
        let fileNames = Data.List.map DirectoryServer.fileName fileMappings
        let fileNames' = Data.List.nub $ Data.List.sort fileNames
        return (encryptDecryptArray (getSessionKeyFromTicket t) fileNames')

openFile :: CommonServer.Ticket -> String -> ApiHandler CommonServer.File
openFile t fn = liftIO $ do
    flag <- isNotValidTicket t
    if flag then do
        logError "DirectoryServer" "Session Timed out"
        return (CommonServer.File fn (encryptDecrypt (getSessionKeyFromTicket t) "Timeout"))
    else do
        logConnection "" "DirectoryServer" "GET open"
        fileMapping <- findFileMapping fn
        if lock fileMapping /= FileLocked then do
            file <- downloadFromFileServer t fn $ identity fileMapping
            updateFileMapping fn FileReadOnly
            logTrailing
            return file
        else do
            logTrailing
            return (CommonServer.File fn (encryptDecrypt (getSessionKeyFromTicket t) "FileLocked"))


closeFile :: CommonServer.Ticket -> CommonServer.File -> ApiHandler CommonServer.Response
closeFile t f = liftIO $ do
    flag <- isNotValidTicket t
    if flag then do
        logError "DirectoryServer" "Session Timed out"
        return (CommonServer.Response CommonServer.DirectoryError directoryServerIdentity "")
    else do
        logConnection "" "DirectoryServer" "POST close"
        let fn = CommonServer.fileName f
        fileMapping <- findFileMapping fn
        if lock fileMapping == FileUnlocked then do
            response <- uploadToFileServer t f (identity fileMapping)
            updateFileMapping fn FileUnlocked
            logTrailing
            return response
        else do
            logTrailing
            return (CommonServer.Response CommonServer.FileLockedOrReadOnly directoryServerIdentity "")

joinServer :: CommonServer.Identity -> ApiHandler CommonServer.Response
joinServer i = liftIO $ do
    logConnection "" "DirectoryServer" "POST join"
    upsertFileServer i
    logTrailing
    return (CommonServer.Response CommonServer.DirectoryJoinSuccess directoryServerIdentity "")



