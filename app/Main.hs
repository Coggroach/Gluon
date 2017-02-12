module Main where

import System.Environment
import CommonServer
import DirectoryServer
import FileServer
import SecurityServer

fileServerCompare :: String
fileServerCompare = "FileServer"

directoryServerCompare :: String
directoryServerCompare = "DirectoryServer"

securityServerCompare :: String
securityServerCompare = "SecurityServer"

runSelectedApp :: String -> IO()
runSelectedApp s = case () of
    () | s == fileServerCompare ->  mkFileServer
       | s == directoryServerCompare -> mkDirectoryServer
       | s == securityServerCompare -> mkSecurityServer

main :: IO ()
main = do
    args <- getArgs
    runSelectedApp (head args)
