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

import Control.Monad.Except
import System.FilePath.Posix
import System.Environment.XDG.BaseDir
import System.Environment.XDG.DesktopEntry hiding (Application)
import System.Environment.XDG.MimeApps
import System.Process (spawnProcess, ProcessHandle)
import System.Directory (findExecutable)


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

-- | Defines basic operation which every executable type shoudl inherit
class Executable a where
    execGetName :: a -> String      -- ^ get the name of the executable
    execGetPath :: a -> FilePath    -- ^ get the path to the executable (or just the plain name)
    exec        :: a -> IO ()       -- ^ run the executable

instance Executable DesktopEntry where
    execGetName d = getName d
    execGetPath d = case getExec d of
                         Just e  -> e
                         Nothing -> getName d
    exec d        = void $ execEntry d

instance Executable Application where
    execGetName (Application app _) = takeBaseName app
    execGetPath (Application app _) = app
    exec app                        = void $ spawn app


-- | Search if a executable exists
findExecPath :: (MonadIO m) => Application -> m (Maybe FilePath)
findExecPath (Application app _) = liftIO $ findExecutable app

-- | Spawns a application
spawn :: (MonadIO m) =>  Application -> m (Maybe ProcessHandle)
spawn a@(Application app args) = do
        exec <- findExecPath a
        case exec of 
         Nothing -> return Nothing
         Just e -> do
                    hdl <- liftIO $ spawnProcess e args
                    return $ Just hdl

