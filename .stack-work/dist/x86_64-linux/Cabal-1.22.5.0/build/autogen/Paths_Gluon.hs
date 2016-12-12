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

bindir     = "/home/ggunn/Gluon/.stack-work/install/x86_64-linux/nightly-2016-04-14/7.10.3/bin"
libdir     = "/home/ggunn/Gluon/.stack-work/install/x86_64-linux/nightly-2016-04-14/7.10.3/lib/x86_64-linux-ghc-7.10.3/Gluon-0.1.0.0-DIc09izJf3WGBTiZZlClK0"
datadir    = "/home/ggunn/Gluon/.stack-work/install/x86_64-linux/nightly-2016-04-14/7.10.3/share/x86_64-linux-ghc-7.10.3/Gluon-0.1.0.0"
libexecdir = "/home/ggunn/Gluon/.stack-work/install/x86_64-linux/nightly-2016-04-14/7.10.3/libexec"
sysconfdir = "/home/ggunn/Gluon/.stack-work/install/x86_64-linux/nightly-2016-04-14/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Gluon_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Gluon_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Gluon_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Gluon_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Gluon_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
