{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module DirectoryServer where

import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class
import           Data.Aeson
import         	 Data.Bson.Generic
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
import           Data.Map (Map)
import           Data.Time
import           System.Random
import qualified Data.Map as M
import           Network.HTTP.Client (newManager, defaultManagerSettings)
import           MongoDbConnector


resources :: Resources
resources = Resources "res/DirectoryServer";

directoryServer :: Server DirectoryApi
directoryServer = 
    getFiles :<|>
    getFilesFrom :<|>
    openFile :<|>
    closeFile :<|>
    joinServer

directoryApp :: Application 
directoryApp = serve directoryApi directoryServer

mkDirectoryServer :: IO()
mkDirectoryServer = do
    createDirectoryIfMissing True dir
    setCurrentDirectory dir
    run (getIdentityPort directoryServerIdentity) directoryApp

getFiles :: ApiHandler [FilePath]

getFilesFrom :: CommonServer.Identity -> ApiHandler [FilePath] 

openFile :: String -> ApiHandler CommonServer.File

closeFile :: CommonServer.File -> ApiHandler CommonServer.Response

joinServer :: CommonServer.Identity -> ApiHandler CommonServer.Response
joinServer i = liftIO $ withMongoDbConnection $ upsert (select ["_id" =: key] "FILESERVER_RECORD") $ toBSON i



