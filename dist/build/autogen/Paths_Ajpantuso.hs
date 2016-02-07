module Paths_Ajpantuso (
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

bindir     = "/home/andrew/.cabal/bin"
libdir     = "/home/andrew/.cabal/lib/x86_64-linux-ghc-7.10.3/Ajpantuso-0.1.0.0-0EBGPLNjDfU3q14S37eC8y"
datadir    = "/home/andrew/.cabal/share/x86_64-linux-ghc-7.10.3/Ajpantuso-0.1.0.0"
libexecdir = "/home/andrew/.cabal/libexec"
sysconfdir = "/home/andrew/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Ajpantuso_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Ajpantuso_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Ajpantuso_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Ajpantuso_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Ajpantuso_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
