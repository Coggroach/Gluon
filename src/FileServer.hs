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
import           CommonServerApiClient

fileIdentity :: CommonServer.Identity

getFileIdentity :: CommonServer.Identity
getFileIdentity = do
    ip <- getNetworkIpAddress 0
    submit <- identityClientSubmitHelper (CommonServer.Identity ip "" CommonServer.FileServer)
    portResponse <- identityClientPortHelper CommonServer.FileServer
    return (CommonServer.Identity ip (payload fileIdentity) CommonServer.FileServer)

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
    fileIdentity <- getFileIdentity
    directoryIdentity <- head identityClientGetHelper CommonServer.DirectoryServer
    putStrLn "Attempting to Join DirectoryServer..."
    manager <- newManager defaultManagerSettings
    response <- runClientM (directoryClientJoin fileIdentity) (ClientEnv manager (BaseUrl Http (address directoryIdentity) (port directoryIdentity) ""))    
    case response of
        Left err -> putStrLn "Error: " ++ show err
        Right response -> run (read (port fileIdentity)::Int) fileApp 

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
    return (CommonServer.Response CommonServer.FileUploadComplete fileIdentity "")