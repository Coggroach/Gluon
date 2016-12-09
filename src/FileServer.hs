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

identity = Identity "localhost" "8081"

resources :: Resources
resources = "res/FileServers";

fileApi :: Proxy FileApi
fileApi = Proxy

server :: Server FileApi
server = 
    getFiles :<|>
    downloadFile :<|>
    uploadFile

fileApp :: Application
fileApp = serve fileApi server

mkApp :: IO()
mkApp = run (read port identity) fileApp 

getFiles :: ApiHandler [FilePath]
getFiles = liftIO (getDirectoryContents resources)

downloadFile :: FilePath -> ApiHandler File
downloadFile f = do    
    content <- liftIO (readFile (resources ++ "/" ++ f))
    return (File f content)

uploadFile :: File -> ApiHandler File
uploadFile (File f c) = do    
    liftIO (writeFile (resources ++ "/" ++ f) c)
    return (File f c)



