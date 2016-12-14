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

fileIdentity = Identity (getNetworkIpAddress 0) "" FileServer

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
mkFileServer = run (read (port fileIdentity)::Int) fileApp 

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

--fsToDsHandshake :: IO()
--fsToDsHandshake = do
   -- manager <- newManager defaultManagerSettings
   -- result <- runClientM identity (ClientEnv manager (BaseUrl Http "localhost" 8081 ""))
   -- case result of
    --    Left err -> putStrLn $ "Error: " ++ show err
     --   Right (Response code id) ->
      --      case code of
       --         HandshakeSuccessful -> return ()
        --        HandshakeError -> fsToDsHandshake


