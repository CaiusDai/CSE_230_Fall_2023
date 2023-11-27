{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_demo1 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/workspaces/CSE_230_Fall_2023/demo1/.stack-work/install/x86_64-linux/3af379905b5fc33534d6fb252a2f15fb5a6a3f848d43b1ddd0a66ea6b59cf1f4/9.2.7/bin"
libdir     = "/workspaces/CSE_230_Fall_2023/demo1/.stack-work/install/x86_64-linux/3af379905b5fc33534d6fb252a2f15fb5a6a3f848d43b1ddd0a66ea6b59cf1f4/9.2.7/lib/x86_64-linux-ghc-9.2.7/demo1-0.1.0.0-47q7xsCpfdIAAxfthzAF6v-demo1"
dynlibdir  = "/workspaces/CSE_230_Fall_2023/demo1/.stack-work/install/x86_64-linux/3af379905b5fc33534d6fb252a2f15fb5a6a3f848d43b1ddd0a66ea6b59cf1f4/9.2.7/lib/x86_64-linux-ghc-9.2.7"
datadir    = "/workspaces/CSE_230_Fall_2023/demo1/.stack-work/install/x86_64-linux/3af379905b5fc33534d6fb252a2f15fb5a6a3f848d43b1ddd0a66ea6b59cf1f4/9.2.7/share/x86_64-linux-ghc-9.2.7/demo1-0.1.0.0"
libexecdir = "/workspaces/CSE_230_Fall_2023/demo1/.stack-work/install/x86_64-linux/3af379905b5fc33534d6fb252a2f15fb5a6a3f848d43b1ddd0a66ea6b59cf1f4/9.2.7/libexec/x86_64-linux-ghc-9.2.7/demo1-0.1.0.0"
sysconfdir = "/workspaces/CSE_230_Fall_2023/demo1/.stack-work/install/x86_64-linux/3af379905b5fc33534d6fb252a2f15fb5a6a3f848d43b1ddd0a66ea6b59cf1f4/9.2.7/etc"

getBinDir     = catchIO (getEnv "demo1_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "demo1_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "demo1_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "demo1_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "demo1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "demo1_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
