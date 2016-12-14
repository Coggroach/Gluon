{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module DirectoryServer where

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
import Data.Map (Map)
import Data.Time
import System.Random
import qualified Data.Map as M
import Network.HTTP.Client (newManager, defaultManagerSettings)

data Filemapping = Filemapping{
	filename :: String,
    fmaddress :: String,
    fmport :: String
}

data Fileserver = Fileserver{
	fsaddress :: String,
	fsport :: String,
	servertype :: ServerType
}

--data Directoryserver = Directoryserver{
--      filemappings   :: TVar (M.Map Filename Filemapping)
--    , fileservers :: TVar (M.Map Uuid Fileserver)
--    , fileservercount :: TVar Int
-- }

filemappings :: [Filemapping]
filemappings = []

fileservers :: [CommonServer.Identity]
fileservers = []

directoryApi :: Proxy DirectoryApi
directoryApi = Proxy

directoryServer :: Server DirectoryApi
directoryServer = 
    file :<|>
    files :<|>
    open -- :<|>
 --   close :<|>
 --   beginTrans :<|>
 --   endTrans :<|>
 --   commitTrans

mkDirectoryServer :: Application
mkDirectoryServer = serve DirectoryServer.directoryApi directoryServer

submitquery :: ClientM CommonServer.Response
submitquery = idsubmit (Identity "localhost" "8082" DirectoryServer)  

getfsquery :: ClientM [Identity]
getfsquery = idall FileServer  

getinitfiles :: CommonServer.Identity -> [Filemapping]
getinitfiles  i = do
   manager <- newManager defaultManagerSettings
   res <- runClientM fsfiles (ClientEnv manager (BaseUrl Http (address i) (read(port i)) "files"))
   case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right response -> do
    	map (newfilemap (address i) (port i)) response
    	putStrLn "Files found successfully"
   return filemappings

newfilemap :: String -> String -> FilePath -> IO()
newfilemap address port fname = do
	filemap <- Filemapping fname address port 
	filemappings ++ [filemap]
	return()

mkdirectoryServer :: IO()
mkdirectoryServer = do 
  manager <- newManager defaultManagerSettings
  res <- runClientM submitquery (ClientEnv manager (BaseUrl Http "localhost" 8081 "submit"))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right response -> print response
  fs <- runClientM getfsquery (ClientEnv manager (BaseUrl Http "localhost" 8081 "all"))
  case fs of
   	Left err -> putStrLn $ "Error: " ++ show err
   	Right response -> do
   		fileservers <- response
   		putStrLn "FileServers Received Successfully"
  
  map getinitfiles fileservers

  run 8082 directoryApp

getFilenamesRecursive :: String -> [Filemapping]-> [String]
getFilenamesRecursive 0  ff = ff
getFilenamesRecursive i ff = do
    id  <- ff !! (i-1)    
    ff ++ filename id
    return getFilenamesRecursive (i-1)

filenames :: [String]
filenames = []

getFilenames :: [Filemapping] -> [String]
getFilenames ff = do
    len  <- length ff
    if length ff /= len then do
        ff <- []
        getFilenamesRecursive len
    else ff 


file :: ApiHandler [String]
file = getFilenames filemappings


files :: String -> ApiHandler [String]
files s = getFilenames (filter (\n -> fmaddress n == s) filemappings)
	

open :: String -> ApiHandler File
open = getFilewFname

getFilewFname :: String -> File
getFilewFname fname = do
    manager <- newManager defaultManagerSettings
    fmapping <- liftIO (filter (\n -> filename n == fname) filemappings)
    runClientM (filename fmapping) (ClientEnv manager (BaseUrl Http (address fmapping) read(port fmapping) "download"))


--close :: File -> ApiHandler CommonServer.Response
--close file = do




