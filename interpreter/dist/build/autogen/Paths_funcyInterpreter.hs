{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_funcyInterpreter (
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

bindir     = "/home/abastro/.cabal/bin"
libdir     = "/home/abastro/.cabal/lib/x86_64-linux-ghc-8.0.2/funcyInterpreter-0.1.0.0-71T9jK7hgc925Tcx7203KI"
dynlibdir  = "/home/abastro/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/abastro/.cabal/share/x86_64-linux-ghc-8.0.2/funcyInterpreter-0.1.0.0"
libexecdir = "/home/abastro/.cabal/libexec"
sysconfdir = "/home/abastro/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "funcyInterpreter_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "funcyInterpreter_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "funcyInterpreter_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "funcyInterpreter_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "funcyInterpreter_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "funcyInterpreter_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
