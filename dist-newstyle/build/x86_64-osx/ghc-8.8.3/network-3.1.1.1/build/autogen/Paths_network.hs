{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_network (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
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
version = Version [3,1,1,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/zhangyunfeng/.cabal/bin"
libdir     = "/Users/zhangyunfeng/.cabal/lib/x86_64-osx-ghc-8.8.3/network-3.1.1.1-inplace"
dynlibdir  = "/Users/zhangyunfeng/.cabal/lib/x86_64-osx-ghc-8.8.3"
datadir    = "/Users/zhangyunfeng/.cabal/share/x86_64-osx-ghc-8.8.3/network-3.1.1.1"
libexecdir = "/Users/zhangyunfeng/.cabal/libexec/x86_64-osx-ghc-8.8.3/network-3.1.1.1"
sysconfdir = "/Users/zhangyunfeng/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "network_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "network_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "network_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "network_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "network_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "network_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
