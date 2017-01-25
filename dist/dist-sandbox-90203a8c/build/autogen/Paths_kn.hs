module Paths_kn (
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

bindir     = "/home/niels/Hacking/Haskell/kn-/.cabal-sandbox/bin"
libdir     = "/home/niels/Hacking/Haskell/kn-/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.3/kn-0.1.0.0-3HeeWqP1QZxCb6NHC6963t"
datadir    = "/home/niels/Hacking/Haskell/kn-/.cabal-sandbox/share/x86_64-linux-ghc-7.10.3/kn-0.1.0.0"
libexecdir = "/home/niels/Hacking/Haskell/kn-/.cabal-sandbox/libexec"
sysconfdir = "/home/niels/Hacking/Haskell/kn-/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "kn_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "kn_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "kn_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "kn_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "kn_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
