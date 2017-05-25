{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_client_proxy (
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

bindir     = "C:\\Users\\HOME\\Desktop\\project\\client-proxy\\.stack-work\\install\\be9a2133\\bin"
libdir     = "C:\\Users\\HOME\\Desktop\\project\\client-proxy\\.stack-work\\install\\be9a2133\\lib\\i386-windows-ghc-8.0.1\\client-proxy-0.1.0.0-LyViFaiq3545JbiIEj64e8"
datadir    = "C:\\Users\\HOME\\Desktop\\project\\client-proxy\\.stack-work\\install\\be9a2133\\share\\i386-windows-ghc-8.0.1\\client-proxy-0.1.0.0"
libexecdir = "C:\\Users\\HOME\\Desktop\\project\\client-proxy\\.stack-work\\install\\be9a2133\\libexec"
sysconfdir = "C:\\Users\\HOME\\Desktop\\project\\client-proxy\\.stack-work\\install\\be9a2133\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "client_proxy_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "client_proxy_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "client_proxy_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "client_proxy_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "client_proxy_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
