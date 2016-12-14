module Main where

import CommonServer
import DirectoryServer
import IdentityServer
import FileServer

runSelectedApp :: CommonServer.ServerType -> IO()
runSelectedApp CommonServer.FileServer = mkFileServer
runSelectedApp CommonServer.DirectoryServer = mkDirectoryServer
runSelectedApp CommonServer.IdentityServer = mkIdentityServer

main :: IO ()
main = runSelectedApp getArgs
