module Main where

import CommonServer
import DirectoryServer
import FileServer

runSelectedApp :: CommonServer.ServerType -> IO()
runSelectedApp CommonServer.FileServer = mkFileServer
runSelectedApp CommonServer.DirectoryServer = mkDirectoryServer

main :: IO ()
main = runSelectedApp getArgs
