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

mkFileServer :: IO()
mkFileServer = do
    let dir = path resources
    createDirectoryIfMissing True dir
    setCurrentDirectory dir
    logHeading "FileServer"    
    logConnection "FileServer" "DirectoryServer" "POST join"
    manager <- newManager defaultManagerSettings
    response <- runClientM (directoryClientJoin fileServerIdentity) (ClientEnv manager (BaseUrl Http (address directoryServerIdentity) (getIdentityPort directoryServerIdentity) ""))    
    case response of
        Left err -> logError "FileServer" $ show err
        Right response -> logAction "FileServer" "Done" ""
    logAction "FileServer" "Start" $ show (getIdentityString directoryServerIdentity)
    run (getIdentityPort fileServerIdentity) fileApp 

------------------------------
--  Serving Functions
------------------------------
getFiles :: ApiHandler [FilePath]
getFiles = liftIO $ do
    logConnection "" "FileServer" "GET files"
    dir <- getCurrentDirectory
    listDirectory dir

downloadFile :: String -> ApiHandler File
downloadFile fn = liftIO $ do
    logConnection "" "FileServer" "GET download"
    content <- liftIO (readFile fn)
    return (File fn content)

uploadFile :: File -> ApiHandler CommonServer.Response
uploadFile (File f c) = liftIO $ do
    logConnection "" "FileServer" "POST upload"
    liftIO (writeFile f c)
    return (CommonServer.Response CommonServer.FileUploadComplete fileServerIdentity "")