{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_damas (
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

bindir     = "/workspaces/jogo-dama-heskel/.stack-work/install/x86_64-linux-tinfo6/bbfe51c8a86770cb8722869941bbfecdb7bb423f66aee193423961c8ade6a503/8.10.7/bin"
libdir     = "/workspaces/jogo-dama-heskel/.stack-work/install/x86_64-linux-tinfo6/bbfe51c8a86770cb8722869941bbfecdb7bb423f66aee193423961c8ade6a503/8.10.7/lib/x86_64-linux-ghc-8.10.7/damas-0.1.0.0-HwIx14Bcj7yHRm4DfXbCcp-damas"
dynlibdir  = "/workspaces/jogo-dama-heskel/.stack-work/install/x86_64-linux-tinfo6/bbfe51c8a86770cb8722869941bbfecdb7bb423f66aee193423961c8ade6a503/8.10.7/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/workspaces/jogo-dama-heskel/.stack-work/install/x86_64-linux-tinfo6/bbfe51c8a86770cb8722869941bbfecdb7bb423f66aee193423961c8ade6a503/8.10.7/share/x86_64-linux-ghc-8.10.7/damas-0.1.0.0"
libexecdir = "/workspaces/jogo-dama-heskel/.stack-work/install/x86_64-linux-tinfo6/bbfe51c8a86770cb8722869941bbfecdb7bb423f66aee193423961c8ade6a503/8.10.7/libexec/x86_64-linux-ghc-8.10.7/damas-0.1.0.0"
sysconfdir = "/workspaces/jogo-dama-heskel/.stack-work/install/x86_64-linux-tinfo6/bbfe51c8a86770cb8722869941bbfecdb7bb423f66aee193423961c8ade6a503/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "damas_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "damas_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "damas_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "damas_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "damas_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "damas_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
