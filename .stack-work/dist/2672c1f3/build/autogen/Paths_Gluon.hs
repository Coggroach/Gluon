module Paths_Gluon (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "E:\\Development\\Haskell\\Projects\\Gluon\\.stack-work\\install\\09641122\\bin"
libdir     = "E:\\Development\\Haskell\\Projects\\Gluon\\.stack-work\\install\\09641122\\lib\\x86_64-windows-ghc-7.10.3\\Gluon-0.1.0.0-F6zkNhHVua2ICFHehfGuqk"
datadir    = "E:\\Development\\Haskell\\Projects\\Gluon\\.stack-work\\install\\09641122\\share\\x86_64-windows-ghc-7.10.3\\Gluon-0.1.0.0"
libexecdir = "E:\\Development\\Haskell\\Projects\\Gluon\\.stack-work\\install\\09641122\\libexec"
sysconfdir = "E:\\Development\\Haskell\\Projects\\Gluon\\.stack-work\\install\\09641122\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Gluon_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Gluon_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Gluon_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Gluon_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Gluon_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
