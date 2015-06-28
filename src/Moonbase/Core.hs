{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Moonbase.Core
  ( Moonbase(..)
  , eval
  , fork
  , timeout

  , Message(..)
  , Signal(..)

  , Options(..)
  , isVerbose, cmd, args

  , Runtime(..)
  , dbus, options, signals, terminal, actions
  , mkRuntime

  , MoonbaseAction(..)
  --, actionName, actionGroup, actionHelp, action

  , Moon
  , Terminal
  , Name
  , DBusClient
  , Action

  , get, ask
  , liftIO
  ) where

import DBus hiding (Message, Signal)
import DBus.Client

import qualified Data.Map as M

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import Control.Lens
import Control.Applicative
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import Control.Concurrent

import qualified System.Timeout as T
import Data.Monoid


type Name = String
type DBusClient = Client


newtype Moonbase st a = Moonbase { runMoon :: ReaderT (TVar st) IO a }
  deriving (Functor, Monad, MonadIO, MonadReader (TVar st))

instance Applicative (Moonbase st) where
    pure  = return
    (<*>) = ap

instance (Monoid a) => Monoid (Moonbase st a) where
    mempty = return mempty
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

eval :: (TVar st) -> Moonbase st a -> IO a
eval rt (Moonbase f) = runReaderT f rt 

fork :: Moonbase st () -> Moonbase st ()
fork f = do
  rt <- ask
  void $ liftIO $ forkIO (eval rt f)

timeout :: Int -> Moonbase st a -> Moonbase st (Maybe a)
timeout t f = do
  rt <- ask
  liftIO $ T.timeout t (eval rt f)
  

data Message = Warning
                 | Info
                 | Success
                 | Debug
                 deriving (Show, Eq, Enum)

data Signal = SignalShutdown
                | SignalMessage Message String
                | SignalFatalError String

data Options = Options
  { _isVerbose :: Bool
  , _cmd       :: String
  , _args      :: [String] }
  deriving (Show)

makeLenses ''Options

{-

 > moonbase start
 > moonbase prompt top show
 > moonbase list-actions

 on PromptAction showToggleHelp "show" $ do
   foo <- barz mark
   moep .= dup

 trigger PromptAction "show" []
  

-}


data MoonbaseAction st = MoonbaseAction
  { _actionName  :: Name
  , _actionHelp  :: String
  , _action      :: ([String] -> Moonbase st String) }
  
makeLenses ''MoonbaseAction


data Runtime = Runtime
  { _dbus     :: DBusClient
  , _options  :: Options
--  , _prefered :: Maybe Preferred
--  , _hooks    :: M.Map Name Hook
  , _signals  :: TQueue Signal
  , _terminal :: Maybe String -> Moonbase Runtime String
  , _actions  :: M.Map String (MoonbaseAction Runtime)
  }

makeLenses ''Runtime

type Action     = MoonbaseAction Runtime
type Moon a     = Moonbase Runtime a
type Terminal   = (Maybe String -> Moon String)

mkRuntime :: DBusClient -> Options -> Terminal -> IO (TQueue Signal, TVar Runtime)
mkRuntime dbusClient opts term = do
    sigs <- atomically newTQueue
    runtime <- atomically $ newTVar (new sigs)
    return (sigs, runtime)
  where
    new sigs = Runtime dbusClient opts sigs term M.empty
