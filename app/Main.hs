module Main where

import System.Environment
import CommonServer
import DirectoryServer
import FileServer

fileServerCompare :: String
fileServerCompare = "FileServer"

directoryServerCompare :: String
directoryServerCompare = "DirectoryServer"

runSelectedApp :: String -> IO()
runSelectedApp s = case () of
    () | s == fileServerCompare ->  mkFileServer
       | s == directoryServerCompare -> mkDirectoryServer

main :: IO ()
main = do
    args <- getArgs
    runSelectedApp (head args)
