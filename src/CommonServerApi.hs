{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module CommonServerApi where

import           Control.Monad.Trans.Except
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
    "files" :> ReqBody '[JSON] CommonServer.Ticket :> Post '[JSON] [FilePath] :<|>
    "open" :> ReqBody '[JSON] CommonServer.Ticket :> ReqBody '[JSON] String :> Post '[JSON] CommonServer.File :<|>
    "close" :> ReqBody '[JSON] CommonServer.Ticket :> ReqBody '[JSON] CommonServer.File :> Post '[JSON] CommonServer.Response :<|>
    "join" :> ReqBody '[JSON] CommonServer.Identity :> Post '[JSON] CommonServer.Response

directoryApi :: Proxy DirectoryApi
directoryApi = Proxy

directoryClientFiles :: CommonServer.Ticket -> ClientM [FilePath]
directoryClientOpen :: CommonServer.Ticket -> String -> ClientM  CommonServer.File
directoryClientClose :: CommonServer.Ticket -> CommonServer.File -> ClientM CommonServer.Response
directoryClientJoin :: CommonServer.Identity -> ClientM CommonServer.Response

directoryClientFiles :<|> directoryClientOpen:<|> directoryClientClose :<|> directoryClientJoin = Servant.Client.client directoryApi

------------------------------
--  SecurityServer Api
------------------------------
type SecurityApi =
    "login" :> ReqBody '[JSON] CommonServer.EncryptedClient :> Post '[JSON] CommonServer.Session :<|>
    "register" :> ReqBody '[JSON] CommonServer.Client :> Post '[JSON] CommonServer.Response :<|>
    "contains" :> Capture "clientName" String :> Get '[JSON] CommonServer.Response 

securityApi :: Proxy SecurityApi
securityApi = Proxy

securityClientLogin :: CommonServer.EncryptedClient -> ClientM CommonServer.Session
securityClientRegister :: CommonServer.Client -> ClientM CommonServer.Response
securityClientContains :: String -> ClientM CommonServer.Response

securityClientLogin :<|> securityClientRegister :<|> securityClientContains = Servant.Client.client securityApi
    
------------------------------
--  Proxy Api
------------------------------
type ProxyApi =
    "login" :> ReqBody '[JSON] CommonServer.ClientRequest :> Post '[JSON] CommonServer.Response :<|>
    "files" :> ReqBody '[JSON] CommonServer.ClientRequest :> Post '[JSON] [FilePath] :<|>
    "open"  :> ReqBody '[JSON] CommonServer.ClientRequest :> Post '[JSON] CommonServer.File :<|>
    "close" :> ReqBody '[JSON] CommonServer.ClientFileRequest :> Post '[JSON] CommonServer.Response :<|>
    "begin" :> ReqBody '[JSON] CommonServer.ClientRequest :> Post '[JSON] CommonServer.Response :<|>
    "end"   :> ReqBody '[JSON] CommonServer.ClientRequest :> Post '[JSON] CommonServer.Response

proxyApi :: Proxy ProxyApi
proxyApi = Proxy

------------------------------
--  TransactionServer Api
------------------------------
type TransactionApi =
    "beginT" :> ReqBody '[JSON] Ticket :> Post '[JSON] CommonServer.Response :<|>
    "downloadT" :> ReqBody '[JSON] CommonServer.SessionString :> Post '[JSON] CommonServer.File :<|>
    "uploadT" :> ReqBody '[JSON] CommonServer.SessionFile :> Post '[JSON] CommonServer.Response :<|>
    "endT" :> ReqBody '[JSON] Ticket :> Post '[JSON] CommonServer.Response :<|>
    "statusT" :> ReqBody '[JSON] Ticket :> Post :> Get '[JSON] CommonServer.Response

transactionApi :: Proxy TransactionApi
transactionApi = Proxy
