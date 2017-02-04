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
    identity :: CommonServer.Identity
} deriving (Eq, Show, Generic,ToJSON, FromJSON, ToBSON, FromBSON)

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
    putStrLn $ "Starting DirectoryServer: " ++ getIdentityString directoryServerIdentity
    deleteDatabases
    run (getIdentityPort directoryServerIdentity) directoryApp

------------------------------
--  Helper Functions
------------------------------
deleteDatabases :: IO ()
deleteDatabases = liftIO $ do
    putStrLn "Clearing: FileMappingDb, FileServerDb"
    connectToDatabase $ do
        Database.MongoDB.delete (select [] "FileMappingDb")
        Database.MongoDB.delete (select [] "FileServerDb")

upsertFileMapping :: CommonServer.Identity -> [FileMapping] -> String -> IO [FileMapping]
upsertFileMapping id array filename = do
    putStrLn $ "Storing FileMapping: " ++ filename
    let filemapping = FileMapping filename id
    connectToDatabase $ upsert (select ["_id" =: filename] "FileMappingDb") $ toBSON filemapping
    return $ FileMapping filename id : array

upsertFileServer :: CommonServer.Identity -> IO()
upsertFileServer i = liftIO $ do
    let key = getIdentitySafeString i
    putStrLn $ "Storing FileServer: " ++ key
    connectToDatabase $ Database.MongoDB.upsert (Database.MongoDB.select ["_id" =: key] "FileServerDb") $ toBSON i
    putStrLn "Success"

getFilesFromFileServer :: CommonServer.Identity -> IO()
getFilesFromFileServer i = liftIO $ do
    manager <- newManager defaultManagerSettings
    response <- runClientM fileClientFiles (ClientEnv manager (BaseUrl Http (address i) (read(port i)::Int) ""))
    case response of
        Left err -> putStrLn $ "Error: " ++ show err
        Right response' -> do
            mapM_ (upsertFileMapping i []) response'
            return ()

findFileMapping :: String -> IO FileMapping
findFileMapping key = do
    putStrLn $ "Searching with Key: " ++ key
    filemapping <- connectToDatabase $ do
        docs <- Database.MongoDB.find (Database.MongoDB.select ["_id" =: key] "FileMappingDb") >>= drainCursor
        return $ Data.Maybe.mapMaybe (\ b -> fromBSON b :: Maybe FileMapping) docs
    return $ head filemapping

getAllFileServers :: IO [CommonServer.Identity]
getAllFileServers = do
    putStrLn "Fetching FileServer List."
    connectToDatabase $ do
        docs <- Database.MongoDB.find (Database.MongoDB.select [] "FileServerDb") >>= drainCursor
        return $ Data.Maybe.mapMaybe (\ b -> fromBSON b :: Maybe CommonServer.Identity) docs

getAllFileMappings :: IO [FileMapping]
getAllFileMappings = do
    putStrLn "Fetching FileMapping List."
    connectToDatabase $ do
        docs <- Database.MongoDB.find (Database.MongoDB.select [] "FileMappingDb") >>= drainCursor
        return $ Data.Maybe.mapMaybe (\ b -> fromBSON b :: Maybe FileMapping) docs

downloadFromFileServer :: String -> CommonServer.Identity -> IO CommonServer.File
downloadFromFileServer fn i = do
    manager <- newManager defaultManagerSettings
    response <- runClientM (fileClientDownload fn) (ClientEnv manager (BaseUrl Http (address i) (read(port i)::Int) ""))
    case response of
        Left err -> return (File "" "")
        Right response' -> return response'

uploadToFileServer :: CommonServer.File -> CommonServer.Identity -> IO CommonServer.Response
uploadToFileServer f i = do
    manager <- newManager defaultManagerSettings
    response <- runClientM (fileClientUpload f) (ClientEnv manager (BaseUrl Http (address i) (read(port i)::Int) ""))
    case response of
        Left err -> return (CommonServer.Response CommonServer.FileUploadError i "")
        Right response' -> return response'

------------------------------
--  Serving Functions
------------------------------
getFiles :: ApiHandler [FilePath]
getFiles = liftIO $ do
    putStrLn "Fetching File List"
    fileServers <- getAllFileServers
    mapM_ getFilesFromFileServer fileServers
    fileMappings <- getAllFileMappings
    let fileNames = Data.List.map DirectoryServer.fileName fileMappings
    return (Data.List.nub $ Data.List.sort fileNames)

openFile :: String -> ApiHandler CommonServer.File
openFile fn = liftIO $ do
    putStrLn $ "Opening File: " ++ fn
    fileMapping <- findFileMapping fn
    downloadFromFileServer fn $ identity fileMapping

closeFile :: CommonServer.File -> ApiHandler CommonServer.Response
closeFile f = liftIO $ do
    putStrLn $ "Closing File: " ++ CommonServer.fileName f
    fileMapping <- findFileMapping (CommonServer.fileName f)
    uploadToFileServer f (identity fileMapping)

joinServer :: CommonServer.Identity -> ApiHandler CommonServer.Response
joinServer i = liftIO $ do
    upsertFileServer i
    return (CommonServer.Response CommonServer.DirectoryJoinSuccess directoryServerIdentity "")



