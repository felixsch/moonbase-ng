{-|
Module      : Moonbase.Util.Application
Copyright   : (c) Felix Schnizlein, 2014
License     : GPL-2
Maintainer  : felix@none.io
Stability   : experimental
Portability : POSIX

Helpers to easily run external applications or execute DesktopEntry files
-}

module Moonbase.Application
    ( Application(..)
    , Argument
    , app
    , appWith
    , Executable(..)
    , findExecPath
    , spawn
    ) where

import           Control.Monad.Except
import           System.Directory                    (findExecutable)
import           System.Environment.XDG.BaseDir
import           System.Environment.XDG.DesktopEntry hiding (Application)
import           System.Environment.XDG.MimeApps
import           System.FilePath.Posix
import           System.Process                      (ProcessHandle,
                                                      spawnProcess)

import           Moonbase.Core

-- | An argument
type Argument = String

-- | Defines a application with a name and some arguments
data Application = Application String [Argument]

-- | Creates a simple application by name without arguments
app :: String -> Application
app exec = Application exec []

-- | Creates a application with arguments
appWith :: String -> [Argument] -> Application
appWith = Application

instance Executable Application where
    execGetName (Application app _) = takeBaseName app
    exec app                        = void $ spawn app


-- | Search if a executable exists
findExecPath :: (Moon m) => Application -> m (Maybe FilePath)
findExecPath (Application app _) = io $ findExecutable app

-- | Spawns a application
spawn :: (Moon m) =>  Application -> m (Maybe ProcessHandle)
spawn a@(Application app args) = do
        exec <- findExecPath a
        case exec of
         Nothing -> return Nothing
         Just e -> do
                    hdl <- io $ spawnProcess e args
                    return $ Just hdl
