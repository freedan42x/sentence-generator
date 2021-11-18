{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_math_functions (
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
version = Version [0,3,4,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/bredor/.cabal/store/ghc-8.6.5/math-functions-0.3.4.0-d628b123dee5468b9c2f3abb0da9df6023f2a1fad8d26a55f3dd2df1d4026011/bin"
libdir     = "/home/bredor/.cabal/store/ghc-8.6.5/math-functions-0.3.4.0-d628b123dee5468b9c2f3abb0da9df6023f2a1fad8d26a55f3dd2df1d4026011/lib"
dynlibdir  = "/home/bredor/.cabal/store/ghc-8.6.5/math-functions-0.3.4.0-d628b123dee5468b9c2f3abb0da9df6023f2a1fad8d26a55f3dd2df1d4026011/lib"
datadir    = "/home/bredor/.cabal/store/ghc-8.6.5/math-functions-0.3.4.0-d628b123dee5468b9c2f3abb0da9df6023f2a1fad8d26a55f3dd2df1d4026011/share"
libexecdir = "/home/bredor/.cabal/store/ghc-8.6.5/math-functions-0.3.4.0-d628b123dee5468b9c2f3abb0da9df6023f2a1fad8d26a55f3dd2df1d4026011/libexec"
sysconfdir = "/home/bredor/.cabal/store/ghc-8.6.5/math-functions-0.3.4.0-d628b123dee5468b9c2f3abb0da9df6023f2a1fad8d26a55f3dd2df1d4026011/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "math_functions_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "math_functions_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "math_functions_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "math_functions_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "math_functions_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "math_functions_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
