module Paths_moonbase (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/felixsch/.cabal/bin"
libdir     = "/home/felixsch/.cabal/lib/x86_64-linux-ghc-7.8.4/moonbase-0.1.0.0"
datadir    = "/home/felixsch/.cabal/share/x86_64-linux-ghc-7.8.4/moonbase-0.1.0.0"
libexecdir = "/home/felixsch/.cabal/libexec"
sysconfdir = "/home/felixsch/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "moonbase_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "moonbase_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "moonbase_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "moonbase_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "moonbase_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
