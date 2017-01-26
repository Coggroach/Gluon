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

resources :: Resources
resources = Resources "res/FileServers";

fileServer :: Server FileApi
fileServer = 
    getFiles :<|>
    downloadFile :<|>
    uploadFile

fileApp :: Application
fileApp = serve fileApi fileServer

mkFileServer :: IO()
mkFileServer = do
    let dir = getFilePath ""
    createDirectoryIfMissing True dir
    setCurrentDirectory dir
    manager <- newManager defaultManagerSettings
    response <- runClientM (directoryClientJoin fileServerIdentity) (ClientEnv manager (BaseUrl Http (address directoryServerIdentity) (getIdentityPort directoryServerIdentity) ""))    
    case response of
        Left err -> putStrLn $ "Error: " ++ show err
        Right response -> run (getIdentityPort fileServerIdentity) fileApp 

getFiles :: ApiHandler [FilePath]
getFiles = liftIO (getDirectoryContents (path resources))

getFilePath :: FilePath -> FilePath
getFilePath f = path resources ++ "/" ++ f

downloadFile :: FilePath -> ApiHandler File
downloadFile f = do    
    content <- liftIO (readFile (getFilePath f))
    return (File f content)

uploadFile :: File -> ApiHandler CommonServer.Response
uploadFile (File f c) = do    
    liftIO (writeFile (getFilePath f) c)
    return (CommonServer.Response CommonServer.FileUploadComplete fileServerIdentity "")