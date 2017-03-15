module Main where

import System.Environment
import CommonServer
import DirectoryServer
import FileServer
import SecurityServer
import ProxyServer

fileServerCompare0 :: String
fileServerCompare0 = "FileServer0"

fileServerCompare1 :: String
fileServerCompare1 = "FileServer1"

fileServerCompare2 :: String
fileServerCompare2 = "FileServer2"

directoryServerCompare :: String
directoryServerCompare = "DirectoryServer"

securityServerCompare :: String
securityServerCompare = "SecurityServer"

proxyServerCompare :: String
proxyServerCompare = "ProxyServer"

runSelectedApp :: String -> IO()
runSelectedApp s = case () of
    () | s == fileServerCompare0 ->  mkFileServer fileServerIdentity0
       | s == fileServerCompare1 ->  mkFileServer fileServerIdentity1
       | s == fileServerCompare2 ->  mkFileServer fileServerIdentity2
       | s == directoryServerCompare -> mkDirectoryServer
       | s == securityServerCompare -> mkSecurityServer
       | s == proxyServerCompare -> mkProxyServer

main :: IO ()
main = do
    args <- getArgs
    runSelectedApp (head args)
