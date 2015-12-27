{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Moonbase.Core
  ( MoonbaseException(..)

  , Moonbase(..)
  , eval
  , fork
  , timeout

  , Preferred(..)
  , Executable(..)
  , preferred

  , Message(..)
  , Signal(..)

  , Options(..)
  , isVerbose, cmd, args

  , Runtime(..)
  , dbus, options, signals, terminal, actions, theme
  , mkRuntime

  , MoonbaseAction(..)
  , actionName, actionHelp, action

  , Moon
  , Terminal
  , Name
  , DBusClient
  , Action

  , get, ask
  , liftIO
  ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.STM                   (atomically)
import qualified Data.Map                            as M
import           Data.Maybe
import           Data.Monoid
import           DBus.Client
import           System.Environment.XDG.DesktopEntry
import qualified System.Timeout                      as T

import           Moonbase.Theme


-- | Name of a service
type Name       = String

-- | A Dbus Connection
type DBusClient = Client

data MoonbaseException = CouldNotOpenDisplay
  deriving (Show)

instance Exception MoonbaseException

-- | The moonbase core type.
-- Every function in moonbase is based on this type.
newtype Moonbase st a = Moonbase { runMoon :: ReaderT (TVar st) IO a }
  deriving (Functor, Monad, MonadIO, MonadReader (TVar st))

instance Applicative (Moonbase st) where
    pure  = return
    (<*>) = ap

instance (Monoid a) => Monoid (Moonbase st a) where
    mempty  = return mempty
    mappend = liftM2 mappend

-- to make it easy to access runtime informations Moon is a instance of
-- MonadState.
instance MonadState st (Moonbase st) where
    get = do
        ref <- ask
        liftIO $ readTVarIO ref
    put sta = do
        ref <- ask
        liftIO $ atomically $ writeTVar ref sta

-- run a moonbase action with an assioated state
eval :: TVar st -> Moonbase st a -> IO a
eval rt (Moonbase f) = runReaderT f rt

-- fork a new moonbase action
fork :: Moonbase st () -> Moonbase st ()
fork f = do
  rt <- ask
  void $ liftIO $ forkIO (eval rt f)

-- run a fork with a timeout assioated
timeout :: Int -> Moonbase st a -> Moonbase st (Maybe a)
timeout t f = do
  rt <- ask
  liftIO $ T.timeout t (eval rt f)


-- | Defines basic operation which every executable type shoudl inherit
class Executable a where
    execGetName :: a -> String              -- ^ get the name of the executable
    execGetPath :: a -> FilePath            -- ^ get the path to the executable (or just the plain name)
    exec        :: (MonadIO m) => a -> m () -- ^ run the executable

instance Executable DesktopEntry where
    execGetName = getName
    execGetPath d = fromMaybe (getName d) (getExec d)
    exec d        = void $ liftIO $ execEntry d


-- | Preferred applications
data Preferred = forall a. (Executable a) => Preferred (M.Map String a)

-- | Message types used in the message channel. Look in Moonbase.Signal for more
-- informations.
data Message = Warning    -- ^ A warning
             | Info       -- ^ An information
             | Success    -- ^ Successfully exectued action
             | Debug      -- ^ Debuging output
             deriving (Show, Eq, Enum)

-- | Signal send trough the message bus.
data Signal = SignalShutdown                   -- ^ signal a shutdown message
                | SignalMessage Message String -- ^ signal a message
                | SignalFatalError String      -- ^ signal a message and than exit gracefully

-- | Options for a moonbase cli action
-- Records can be accessed via the lens types
data Options = Options
  { _isVerbose :: Bool     -- ^ show verbose output if implemented
  , _cmd       :: String   -- ^ the action which could be exectuted
  , _args      :: [String] -- ^ arguments for the command
  } deriving (Show)

makeLenses ''Options


data MoonbaseAction st = MoonbaseAction
  { _actionName :: Name
  , _actionHelp :: String
  , _action     :: [String] -> Moonbase st String }

makeLenses ''MoonbaseAction


-- | The moonbase runtime
data Runtime = Runtime
  { _dbus      :: DBusClient                            -- ^ Dbus client connection
  , _options   :: Options                               -- ^ The called action
  , _preferred :: Maybe Preferred                       -- ^ All preferred applications of the user
--  , _hooks    :: M.Map Name Hook                      -- ^ All defined hooks
  , _signals   :: TQueue Signal                         -- ^ The signal queue
  , _terminal  :: Maybe String -> Moonbase Runtime ()   -- ^ The terminal emulator function
  , _actions   :: M.Map String (MoonbaseAction Runtime) -- ^ All defined actions
  , _theme     :: Theme                                 -- ^ The default theme
  }

makeLenses ''Runtime

-- Wrapper for a MoonbaseAction bound to Runtime
type Action     = MoonbaseAction Runtime

-- Wrapper for Moonbase Runtime
type Moon a     = Moonbase Runtime a

-- A terminal function. First argument is
type Terminal   = (Maybe String -> Moon ())


-- | Generates a new moonbase runtime
mkRuntime :: DBusClient -> Options -> Terminal -> IO (TQueue Signal, TVar Runtime)
mkRuntime dbusClient opts term = do
    sigs <- atomically newTQueue
    runtime <- atomically $ newTVar (new sigs)
    return (sigs, runtime)
  where
    new sigs = Runtime dbusClient opts Nothing sigs term M.empty defaultTheme
