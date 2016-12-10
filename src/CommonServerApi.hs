{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module CommonServerApi where

import           Control.Monad.Trans.Except
import           Data.Aeson
import           Network.Wai
import           Servant
import           CommonServer

type ApiHandler = ExceptT ServantErr IO

type FileApi = 
    "files" :> Get '[JSON] [FilePath] :<|>
    "download" :> Capture "fileName" String :> Get '[JSON] CommonServer.File :<|>
    "upload" :> ReqBody '[JSON] CommonServer.File :> Post '[JSON] CommonServer.Response

type DirectoryApi = 
    "files" :> Get '[JSON] [String] :<|>
    "files" :> Capture "uuid" String :> Get '[JSON] [String] :<|>
    "open" :> Capture "fileName" String :> Get '[JSON] CommonServer.File :<|>
    "close" :> ReqBody '[JSON] CommonServer.File :> Post '[JSON] CommonServer.Response

type HandshakeApi =
    "shake" :> ReqBody '[JSON] CommonServer.Identity :> Post '[JSON] CommonServer.Response :<|>
    "echo" :> Capture "param" String :> Get '[JSON] String

type IdentityApi =
    "submit" :> ReqBody '[JSON] CommonServer.Identity :> Post '[JSON] CommonServer.Response :<|>
    "next" :> ReqBody '[JSON] CommonServer.ServerType :> Post '[JSON] CommonServer.Identity :<|>
    "all" :> ReqBody '[JSON] CommonServer.ServerType :> Post '[JSON] [CommonServer.Identity] :<|>
    "port" :> ReqBody '[JSON] CommonServer.ServerType :> Post '[JSON] Int :<|>
    "report" :> ReqBody '[JSON] CommonServer.Identity :> Post '[JSON] CommonServer.Response

