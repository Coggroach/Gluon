{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module SecurityServer where

import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Bson.Generic
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
import           Data.Map
import           Data.Time
import           Data.List
import           Data.Maybe                   (catMaybes, mapMaybe)
import           Data.Text                    (pack, unpack)
import           System.Random
import           Network.HTTP.Client (newManager, defaultManagerSettings)
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           Database.MongoDB
import           MongoDbConnector

------------------------------
--  Variables
------------------------------
resources :: Resources
resources = Resources "res/SecurityServer";

------------------------------
--  Server Functions
------------------------------
securityServer :: Server SecurityApi
securityServer =
    login :<|>
    register

securityApp :: Application
securityApp = serve securityApi securityServer

mkSecurityServer :: IO()
mkSecurityServer = do
    createDirectoryIfMissing True (path resources)
    setCurrentDirectory (path resources)
    putStrLn $ "Starting SecurityServer: " ++ getIdentityString securityServerIdentity
    run (getIdentityport securityServerIdentity) securityApp

------------------------------
--  Serving Functions
------------------------------

login :: CommonServer.EncryptedClient -> ApiHandler CommonServer.Session
login (CommonServer.EncryptedClient name encryptedData) = liftIO $ do
    putStrLn $ ""