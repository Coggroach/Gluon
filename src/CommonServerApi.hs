{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module CommonServerApi where

import           Control.Monad.Trans.Except
import           Data.Either.Unwrap
import           Data.Aeson
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API
import           Servant.Client
import           CommonServer
import           Network.HTTP.Client (newManager, defaultManagerSettings)

------------------------------
--  ApiHandler
------------------------------
type ApiHandler = ExceptT ServantErr IO

------------------------------
--  FileServer Api 
------------------------------
type FileApi = 
    "files" :> Get '[JSON] [FilePath] :<|>
    "download" :> Capture "fileName" String :> Get '[JSON] CommonServer.File :<|>
    "upload" :> ReqBody '[JSON] CommonServer.File :> Post '[JSON] CommonServer.Response -- :<|>
--    "beginTrans" :> Get '[JSON] CommonServer.Response :<|>
--    "endTrans" :> Get '[JSON] CommonServer.Response :<|>
--    "commitTrans" :> Get '[JSON] CommonServer.Response

fileApi :: Proxy FileApi
fileApi = Proxy

fileClientFiles :: ClientM [FilePath]
fileClientDownload :: String -> ClientM CommonServer.File
fileClientUpload :: CommonServer.File -> ClientM CommonServer.Response

fileClientFiles :<|> fileClientDownload :<|> fileClientUpload = Servant.Client.client fileApi

------------------------------
--  DirectoryServer Api
------------------------------
type DirectoryApi = 
    "files" :> Get '[JSON] [FilePath] :<|>
    "files" :> ReqBody '[JSON] CommonServer.Identity :> Get '[JSON] [FilePath] :<|>
    "open" :> Capture "fileName" String :> Get '[JSON] CommonServer.File :<|>
    "close" :> ReqBody '[JSON] CommonServer.File :> Post '[JSON] CommonServer.Response :<|>
    "join" :> ReqBody '[JSON] CommonServer.Identity :> Post '[JSON] CommonServer.Response  
 --   "beginTrans" :> Get '[JSON] CommonServer.Response :<|>
 --   "endTrans" :> Get '[JSON] CommonServer.Response :<|>
 --   "commitTrans" :> Get '[JSON] CommonServer.Response

directoryApi :: Proxy DirectoryApi
directoryApi = Proxy

directoryClientFilesA :: ClientM [FilePath]
directoryClientFilesB :: CommonServer.Identity -> ClientM [FilePath]
directoryClientOpen :: String -> ClientM  CommonServer.File
directoryClientClose :: CommonServer.File -> ClientM CommonServer.Response
directoryClientJoin :: CommonServer.Identity -> ClientM CommonServer.Response

directoryClientFilesA :<|> directoryClientFilesB :<|> directoryClientOpen:<|> directoryClientClose :<|> directoryClientJoin = Servant.Client.client directoryApi

------------------------------
--  SecurityServer Api
------------------------------
type SecurityApi =
    "login" :> ReqBody '[JSON] CommonServer.Client :> Post '[JSON] CommonServer.Token :<|>
    "session" :> Capture "id" String :> Get '[JSON] CommonServer.Token

securityApi :: Proxy SecurityApi
securityApi = Proxy
    
------------------------------
--  Proxy Api
------------------------------
type ProxyApi =
    "files" :> Get '[JSON] [String] :<|>
    "open" :> Capture "fileName" String :> Get '[JSON] CommonServer.File :<|>
    "close" :> ReqBody '[JSON] CommonServer.File :> Post '[JSON] CommonServer.Response :<|>
    "close" :> Capture "fileName" String :> Get '[JSON] CommonServer.Response

proxyApi :: Proxy ProxyApi
proxyApi = Proxy

------------------------------
--  TransactionServer Api
------------------------------
type TransactionApi =
    "beginT" :> Get '[JSON] CommonServer.Response :<|>
    "endT" :> Get '[JSON] CommonServer.Response :<|>
    "statusT" :> Get '[JSON] CommonServer.Response

transactionApi :: Proxy TransactionApi
transactionApi = Proxy
