{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Gluon (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "E:\\Development\\Haskell\\Projects\\Gluon\\.stack-work\\install\\48716ccc\\bin"
libdir     = "E:\\Development\\Haskell\\Projects\\Gluon\\.stack-work\\install\\48716ccc\\lib\\x86_64-windows-ghc-8.0.1\\Gluon-0.1.0.0-2UdCqnHtygNFYmtyXl73Wl"
datadir    = "E:\\Development\\Haskell\\Projects\\Gluon\\.stack-work\\install\\48716ccc\\share\\x86_64-windows-ghc-8.0.1\\Gluon-0.1.0.0"
libexecdir = "E:\\Development\\Haskell\\Projects\\Gluon\\.stack-work\\install\\48716ccc\\libexec"
sysconfdir = "E:\\Development\\Haskell\\Projects\\Gluon\\.stack-work\\install\\48716ccc\\etc"

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
