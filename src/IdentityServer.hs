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
import           Servant
import           Servant.API
import           Servant.Client
import           System.IO
import           CommonServer
import           CommonServerApi
import           CommonServerApiClient

identityApi :: Proxy IdentityApi
identityApi = Proxy

identityServer :: Server IdentityApi
identityServer = 
    submit :<|>
    getNext :<|>
    getAll :<|>
    IdentityServer.getPort :<|>
    report

identityApp :: Application
identityApp = serve IdentityServer.identityApi identityServer

mkIdentityServer :: IO()
mkIdentityServer = run getIdentityPort theIdentity identityApp

identities :: [Identity]
identities = [theIdentity]

getPortRecursive :: Int -> [Int]
getPortRecursive 0 = ports
getPortRecursive i = do
    id  <- identities !! (i-1)    
    ports ++ getIdentityPort id
    return getPortRecursive (i-1)

ports :: [Int]
ports = []

getPorts :: [Int]
getPorts = do
    len  <- length identities
    if length ports != len then do
        port <- []
        getPortRecursive len
    else ports        

submit :: Identity -> ApiHandler CommonServer.Response
submit i = do
    identities ++ [i]
    return (Response IdentityReceived identity)

getNext :: ServerType -> ApiHandler Identity
getNext st = liftIO (head (filter (\n -> serverType n == st) identities))

getAll :: ServerType -> ApiHandler [Identity]
getAll st = liftIO (filter (\n -> serverType n == st) identities)

getPort :: ServerType -> ApiHandler Int
getPort st = return (maximum getPorts) + 1

report :: Identity -> ApiHandler CommonServer.Response
report i = do
    result <- i `elem` identities
    if result then return (Response IdentityFound i)
    else return (Response IdentityNotFound i)
