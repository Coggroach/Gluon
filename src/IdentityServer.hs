{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module IdentityServer where

import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class
import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Info
import           Servant
import           Servant.API
import           Servant.Client
import           System.IO
import           CommonServer
import           CommonServerApi
import           CommonServerApiClient

identity :: Identity
identity = Identity "localhost" "8001"

identityApi :: Proxy IdentityApi
identityApi = Proxy

server :: Server IdentityApi
server = 
    submit :<|>
    getNext :<|>
    getAll :<|>
    getPort :<|>
    report

identityApp :: Application
identityApp = serve identityApi server

mkIdentityServer :: IO()
mkIdentityServer = run (read port identity) identityApp

identities :: [(Identity, ServerType)]


submit :: Identity -> ApiHandler Response
submit i = 