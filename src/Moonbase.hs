module Moonbase
  ( moonbase
  , verbose
  , signal
  , warn, say, success
  , module Moonbase.Core
  , module Moonbase.DBus
  , module Moonbase.Pipe
  , module Moonbase.Preferred
  , module Moonbase.Desktop
  , module Moonbase.Application
  , module Moonbase.Util
  , runMoonbaseAction
  ) where

import           Control.Applicative
import           Control.Lens                   hiding (argument, (<.>))
import           Control.Monad
--import Control.Concurrent
import           Control.Monad.STM              (atomically)
--import Control.Concurrent.STM.TVar
import qualified Config.Dyre                    as Dy
import           Control.Concurrent             (forkOS)
import           Control.Concurrent.STM.TQueue

import           Options.Applicative            hiding (action)
--import Options.Applicative.Types


import           System.Directory
import           System.FilePath.Posix
import           System.IO
--import           System.Locale
--import System.Environment
--import System.Environment.XDG.DesktopEntry
import           System.Environment.XDG.BaseDir

import qualified Data.Map                       as M
--import           Data.Maybe
import           Data.Time.Format
import           Data.Time.LocalTime

import           DBus                           hiding (Signal, signal)
import           DBus.Client

import qualified Graphics.UI.Gtk                as Gtk

import           Moonbase.Application
import           Moonbase.Core
import           Moonbase.DBus
import           Moonbase.Desktop
import           Moonbase.Pipe
import           Moonbase.Preferred
import           Moonbase.Signal
import           Moonbase.Util


type DyreStartup = (Maybe String, Terminal, Moon ())

moonbase :: Terminal -> Moon () -> IO ()
moonbase term moon = Dy.wrapMain params (Nothing, term, moon)
  where
    params = Dy.defaultParams {
      Dy.projectName = "moonbase"
    , Dy.realMain    = realMoonbase
    , Dy.showError   = \st msg -> st & _1 .~ Just msg
    , Dy.ghcOpts     = ["-threaded", "-Wall"]
    , Dy.includeCurrentDirectory = True }


parseOptions :: IO Options
parseOptions = execParser full
  where
    full    = info (helper <*> opts) ( fullDesc <>
                                          progDesc "moonbase managament tool" <>
                                          header "moonbase - A desktop environment" )
    opts = Options
      <$> switch (  long "verbose"
                 <> help "Print verbose messages to stdout" )
      <*> strArgument (  metavar "ACTION"
                      <> help actionHelp'
                      <> value "start" )
      <*> many (argument str (  metavar "ARGS..."
                             <> help argsHelp))

    argsHelp    ="Arguments for a action. Show action help by help ACTION"
    actionHelp' = "Run specified action. Show all commands available with list-actions"

getHome :: IO FilePath
getHome = (</>) <$> getUserConfigDir <*> pure "moonbase"

setupHomeDirectory :: IO ()
setupHomeDirectory = do
  dir <- getHome
  exists <- doesDirectoryExist dir

  unless exists $ createDirectory dir

-- | Opens the log file
openLog :: IO Handle
openLog = do
 dir    <- getHome
 exists <- doesFileExist (dir </> "moonbase" <.> "log")

 unless exists $ writeFile (dir </> "moonbase" <.> "log") ""
 openFile (dir </> "moonbase" <.> "log") WriteMode


startDBus :: IO Client
startDBus = do
        client <- connectSession
        name   <- requestName client moonbaseBusName []
        case name of
            NamePrimaryOwner -> return client
            _                -> error "Connection to Session Bus failed. Name allready in use"


formatMessage :: String -> IO String
formatMessage message = do
    date <- formatTime defaultTimeLocale rfc822DateFormat <$> getZonedTime
    return $ "[" ++ date ++ "] " ++ message

basicActions :: Moonbase IO ()
basicActions = do
    on "Quit" helpQuit $ \_ -> do
        signal SignalShutdown
        return "Bye Bye..."

    on ("ListAction", "list-actions") helpListActions $ \_ -> do
      actions' <- use actions
      return $ unlines $ map toLine (M.toList actions')

    on ("RunAction", "run-action") helpRunAction $ \(name:args') -> do
      debug $ "[run-action] calling action: " ++ name
      actions' <- use actions
      case actions' ^? ix name of
        Nothing -> return $ "Command `" ++ name ++ "` not found."
        Just action'  -> runAction action' args'

    on "Spawn" helpSpawn $ \(name:args') -> do
      call <- use terminal
      case name of
        "terminal" -> call Nothing
        _          -> warn $ "Invalid application \"" ++ name ++ "\""
      return ""

  where
    helpQuit        = "Quit moonbase"
    helpSpawn       = "Spawn a moonbase application [Apps available: terminal]"
    helpListActions = "Lists all available actions. Use list-actions <group> if you want only a list about a action group"
    helpRunAction   = "Run a action"

    toLine (key, (Action _ help' _)) = key ++ " - " ++ help'


runMoonbase :: Options -> Terminal -> Moonbase IO () -> IO ()
runMoonbase opts term moon = do

    putStrLn "Starting moonbase..."
    setupHomeDirectory

    putStrLn "Opening log..."
    handle <- openLog
    putStrLn "Start dbus..."
    client   <- startDBus
    putStrLn "Make Runtime..."
    (sigs,runtime) <- mkRuntime client opts term

    putStrLn "Run basic actions and user implementation..."
    io Gtk.initGUI
    eval runtime (basicActions >> moon)
    io $ forkOS Gtk.mainGUI
    loop sigs runtime handle
    io Gtk.mainQuit
    return ()
  where
    loop sigs rt hdl = do
      sig <- atomically $ readTQueue sigs
      handleSignal sig sigs rt hdl

    handleSignal (SignalShutdown)        _ _ _ = putStrLn "Bye..."
    handleSignal (SignalFatalError err)  _ _ _ = putStrLn $ "[FATAL]: " ++ err
    handleSignal (SignalMessage typ msg) s r h = do
      message <- formatMessage (show typ ++ ":" ++ msg)

      hPutStrLn h message >> hFlush h

      when (opts ^. isVerbose) $
        hPutStrLn stderr message >> hFlush stderr

      loop s r h


runMoonbaseAction :: Name -> [String] -> IO (Maybe String)
runMoonbaseAction name args' = do
    client <- connectSession

    reply <- call_ client (methodCall (withObjectPath "Action") (withInterface "Action") (memberName_ "RunAction"))
      { methodCallDestination = Just moonbaseBusName
      , methodCallBody        = [toVariant (name:args')] }

    let (Just reply') = fromVariant (methodReturnBody reply !! 0)

    return $ if (not $ null reply')
                then Just reply'
                else Nothing

realMoonbase :: DyreStartup -> IO ()
realMoonbase (Just err, _, _)    = putStrLn $ "Could not load moonbase: " ++ err
realMoonbase (Nothing, term, moon) = do
    opts <- parseOptions
    liftIO $ print opts
    case (opts ^. cmd) of
      "start" -> runMoonbase opts term moon
      a       -> maybe (return ()) (\x -> putStrLn x) =<< runMoonbaseAction a (opts ^. args)
