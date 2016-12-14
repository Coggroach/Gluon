{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module IdentityServer where

import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Lazy as BString
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API
import           Servant.Client
import           System.IO
import           CommonServer
import           CommonServerApi

identityServer :: Server IdentityApi
identityServer = 
    submit :<|>
    --getNext :<|>
    getAll :<|>
    IdentityServer.getPort -- :<|>
    --report

identityResources :: Resources
identityResources = Resources "res/IdentityServer"

identityApp :: Application
identityApp = serve CommonServerApi.identityApi identityServer

mkIdentityServer :: IO()
mkIdentityServer = do
    writeIdentityJson
    run (getIdentityPort theIdentity) identityApp

getIdentitiesPath :: String
getIdentitiesPath = path identityResources ++ "/Identities.json" 

writeIdentityJson :: IO ()
writeIdentityJson = liftIO (BString.writeFile getIdentitiesPath [theIdentity])    

writeIdentity :: Identity -> IO()
writeIdentity i = do
    is <- readIdentities
    is ++ i
    liftIO (writeFile getIdentitiesPath is)

readIdentities :: IO [Identity]
readIdentities = do
    bytes <- liftIO (BString.readFile getIdentitiesPath)
    decode bytes

getPortRecursive :: [Identity] -> [Int] -> Int -> [Int]
getPortRecursive is ps 0 = ps
getPortRecursive is ps i = do
    id  <- is !! (i-1)    
    ps ++ getIdentityPort id
    return getPortRecursive is ps (i-1)

getPorts :: [Int]
getPorts = do
    is <- readIdentities
    len <- length is
    getPortRecursive is [] len      

submit :: Identity -> ApiHandler CommonServer.Response
submit i = do
    writeIdentity i
    Response IdentityReceived theIdentity

filterIdentitiesRecursive :: IO [Identity] -> ServerType -> Int -> IO [Identity]
filterIdentitiesRecursive fis s 0 = fis
filterIdentitiesRecursive fis s i = do
    id <- head fis
    if serverType id /= s then
        drop 1 fis    
        return (filterIdentitiesRecursive fis s (i-1))
    else
        return (filterIdentitiesRecursive fis s (i-1))

filterIdentities :: ServerType -> IO [Identity]
filterIdentities s = do
    let is = readIdentities
    filterIdentitiesRecursive is s (length is)


--getNext :: ServerType -> ApiHandler Identity
--getNext st = liftIO (head (filterIdentities st))

getAll :: ServerType -> ApiHandler [Identity]
getAll st = liftIO (filterIdentities st)

getPort :: ServerType -> ApiHandler Int
getPort st = return (maximum getPorts) + 1

--report :: Identity -> ApiHandler CommonServer.Response
--report i = liftIO (CommonServer.Response CommonServer.IdentityNotFound i "")--do
--   result <- i `elem` identities
--   if result then CommonServer.Response CommonServer.IdentityFound i ""
--   else CommonServer.Response CommonServer.IdentityNotFound i ""    
