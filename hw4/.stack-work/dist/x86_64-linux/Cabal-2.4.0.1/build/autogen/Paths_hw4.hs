{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hw4 (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/phubbard/Desktop/Winter_2020/CS457/HaskellProjects/hw4/.stack-work/install/x86_64-linux/686ffbe051341b7fb4a80ab42e5ec6a1864d75b8bd6e1b66b46aefecc8bf9a3f/8.6.5/bin"
libdir     = "/home/phubbard/Desktop/Winter_2020/CS457/HaskellProjects/hw4/.stack-work/install/x86_64-linux/686ffbe051341b7fb4a80ab42e5ec6a1864d75b8bd6e1b66b46aefecc8bf9a3f/8.6.5/lib/x86_64-linux-ghc-8.6.5/hw4-0.1.0.0-9XHRsklhAukCwGU3ehOYOn"
dynlibdir  = "/home/phubbard/Desktop/Winter_2020/CS457/HaskellProjects/hw4/.stack-work/install/x86_64-linux/686ffbe051341b7fb4a80ab42e5ec6a1864d75b8bd6e1b66b46aefecc8bf9a3f/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/phubbard/Desktop/Winter_2020/CS457/HaskellProjects/hw4/.stack-work/install/x86_64-linux/686ffbe051341b7fb4a80ab42e5ec6a1864d75b8bd6e1b66b46aefecc8bf9a3f/8.6.5/share/x86_64-linux-ghc-8.6.5/hw4-0.1.0.0"
libexecdir = "/home/phubbard/Desktop/Winter_2020/CS457/HaskellProjects/hw4/.stack-work/install/x86_64-linux/686ffbe051341b7fb4a80ab42e5ec6a1864d75b8bd6e1b66b46aefecc8bf9a3f/8.6.5/libexec/x86_64-linux-ghc-8.6.5/hw4-0.1.0.0"
sysconfdir = "/home/phubbard/Desktop/Winter_2020/CS457/HaskellProjects/hw4/.stack-work/install/x86_64-linux/686ffbe051341b7fb4a80ab42e5ec6a1864d75b8bd6e1b66b46aefecc8bf9a3f/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hw4_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hw4_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hw4_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hw4_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw4_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw4_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
