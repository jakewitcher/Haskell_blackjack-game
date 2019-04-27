{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_blackjack (
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

bindir     = "C:\\Users\\Jake\\Desktop\\Haskell\\blackjack\\.stack-work\\install\\8b47734a\\bin"
libdir     = "C:\\Users\\Jake\\Desktop\\Haskell\\blackjack\\.stack-work\\install\\8b47734a\\lib\\x86_64-windows-ghc-8.6.4\\blackjack-0.1.0.0-Al9OHvAIAwTEuGUDPGFRQH-blackjack"
dynlibdir  = "C:\\Users\\Jake\\Desktop\\Haskell\\blackjack\\.stack-work\\install\\8b47734a\\lib\\x86_64-windows-ghc-8.6.4"
datadir    = "C:\\Users\\Jake\\Desktop\\Haskell\\blackjack\\.stack-work\\install\\8b47734a\\share\\x86_64-windows-ghc-8.6.4\\blackjack-0.1.0.0"
libexecdir = "C:\\Users\\Jake\\Desktop\\Haskell\\blackjack\\.stack-work\\install\\8b47734a\\libexec\\x86_64-windows-ghc-8.6.4\\blackjack-0.1.0.0"
sysconfdir = "C:\\Users\\Jake\\Desktop\\Haskell\\blackjack\\.stack-work\\install\\8b47734a\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "blackjack_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "blackjack_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "blackjack_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "blackjack_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "blackjack_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "blackjack_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
