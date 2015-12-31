{-# LANGUAGE ExistentialQuantification  #-}
--{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
-- {-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Moonbase.Core
  ( Exception(..)
  , Moonbase(..)
  , liftMoon
  , Moon(..)
  , eval, eval'
  , evalWith, evalWith', evalWith_

  , Preferred(..)
  , Executable(..)
  , preferred

  , Message(..)
  , Signal(..)

  , Options(..)
  , isVerbose, cmd, args

  , Runtime(..)
  , dbus, options, signals, terminal, actions, theme

  , Configure(..)
  , configure, configureWith
  --, mkRuntime

  , Action(..)
  , actionName, actionHelp, action

  , Terminal
  , Name
  , DBusClient

  , pushSignal, readSignal
  , get, put, ask
  , getBase
  ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import qualified Control.Exception                   as E
import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
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

data Exception = CouldNotOpenDisplay
               | FileNotFound
               | Shutdown
  deriving (Show)

instance E.Exception Exception


-- | Message types used in the message channel. Look in Moonbase.Signal for more
-- informations.
data Message = Warning    -- ^ A warning
             | Info       -- ^ An information
             | Success    -- ^ Successfully exectued action
             | Debug      -- ^ Debuging output
             deriving (Show, Eq, Enum)

-- | Signal send trough the message bus.
data Signal = SignalMessage Message String  -- ^ signal a message
            | SignalException Exception     -- ^ signal a exception an throw it at main process

class Monad m => Moon m where
  io      :: IO a -> m a
  puts    :: String -> m ()
  fork    :: m () -> m ThreadId
  timeout :: Int -> m a  -> m (Maybe a)

instance Moon IO where
  io      = id
  puts    = putStrLn
  fork    = forkIO
  timeout = T.timeout


type Ref m = TVar (Runtime m)
newtype (Moon m) => Moonbase m a = Moonbase (ExceptT Exception (ReaderT (Ref m) m) a)
  deriving (Functor, Monad, MonadReader (Ref m))

liftMoon :: (Moon m) => m a -> Moonbase m a
liftMoon = Moonbase . lift . lift


-- | The moonbase core type.
-- Every function in moonbase is based on this type.
--newtype Moonbase st a = Moonbase { runMoon :: ReaderT (TVar st) IO a }
--  deriving (Functor, Monad, MonadIO, MonadReader (TVar st))
instance (Moon m) => Applicative (Moonbase m) where
    pure  = return
    (<*>) = ap

instance (Moon m, Monoid a) => Monoid (Moonbase m a) where
    mempty  = return mempty
    mappend = liftM2 mappend

instance (Moon m) => Moon (Moonbase m) where
  io      = liftMoon . io
  puts    = liftMoon . puts
  fork f  = do
    ref <- ask
    liftMoon $ fork (evalWith_ ref f)
  timeout s f = do
    ref <- ask
    liftMoon $ timeout s (evalWith' ref f)

-- to make it easy to access runtime informations Moon is a instance of
-- MonadState.
instance (Moon m) => MonadState (Runtime m) (Moonbase m) where
  get    = io . readTVarIO =<< ask
  put rt = do
    ref <- ask
    io $ atomically $ writeTVar ref rt



-- run a moonbase action with an assioated state
eval :: (Moon m) => Ref m -> Moonbase m a -> m (Either Exception a)
eval ref (Moonbase f) = runReaderT (runExceptT f) ref

eval' :: (Moon m) => Ref m -> Moonbase m a -> m a
eval' ref f = either raiseE return =<< eval ref f
  where
    raiseE ex = error $ "Uncaucht exception: " ++ show ex

evalWith :: (Moon m) => Ref m -> Moonbase m a -> m (Maybe a)
evalWith ref f = either signalE (return . Just) =<< eval ref f
  where
    signalE ex = pushSignal ref (SignalException ex) >> return Nothing

evalWith' :: (Moon m) => Ref m -> Moonbase m a -> m a
evalWith' ref f = maybe raiseE return =<< evalWith ref f
  where
    raiseE = error "Uncaught exception: Unknown exception due evalWith'"

evalWith_ :: (Moon m) => Ref m -> Moonbase m a -> m ()
evalWith_ ref f = void $ evalWith ref f

newtype (Moon m) => Configure c m a = Configure (StateT c m a)
  deriving (Functor, Monad, MonadState c)

instance (Moon m) => Applicative (Configure c m) where
  pure  = return
  (<*>) = ap

liftConfigure ::  (Moon m) => m a -> Configure c m a
liftConfigure = Configure . lift


instance (Moon m) => Moon (Configure c m) where
  io      = liftConfigure . io
  puts    = liftConfigure . puts
  fork f  = do
    st <- get
    liftConfigure (fork $ void $ configure st f)
  timeout ms f = do
    st <- get
    mst <- liftConfigure (timeout ms $ configureWith st f)
    case mst of
      Just (a, st') -> do
        put st'
        return $ Just a
      Nothing -> return Nothing


configure :: (Moon m) => c -> Configure c m () -> m c
configure toConfigure configurator = snd <$> configureWith toConfigure configurator

configureWith :: (Moon m) => c -> Configure c m a -> m (a,c)
configureWith toConfigure (Configure configurator) = runStateT configurator toConfigure

-- | Defines basic operation which every executable type shoudl inherit
class Executable a where
    execGetName :: a -> String              -- ^ get the name of the executable
    execGetPath :: a -> FilePath            -- ^ get the path to the executable (or just the plain name)
    exec        :: (Moon m) => a -> m () -- ^ run the executable

instance Executable DesktopEntry where
    execGetName = getName
    execGetPath d = fromMaybe (getName d) (getExec d)
    exec d        = void $ io $ execEntry d

-- | Preferred applications
data Preferred = forall a. (Executable a) => Preferred (M.Map String a)

-- | Options for a moonbase cli action
-- Records can be accessed via the lens types
data Options = Options
  { _isVerbose :: Bool     -- ^ show verbose output if implemented
  , _cmd       :: String   -- ^ the action which could be exectuted
  , _args      :: [String] -- ^ arguments for the command
  } deriving (Show)


data Action m = MoonbaseAction
  { _actionName :: Name
  , _actionHelp :: String
  , _action     :: [String] -> Moonbase m String }



-- | The moonbase runtime
data Runtime m = Runtime
  { _dbus      :: DBusClient                            -- ^ Dbus client connection
  , _options   :: Options                               -- ^ The called action
  , _preferred :: Maybe Preferred                       -- ^ All preferred applications of the user
--  , _hooks    :: M.Map Name Hook                      -- ^ All defined hooks
  , _signals   :: TQueue Signal                         -- ^ The signal queue
  , _terminal  :: Maybe String -> Moonbase m ()   -- ^ The terminal emulator function
  , _actions   :: M.Map String (Action m ) -- ^ All defined actions
  , _theme     :: Theme                                 -- ^ The default theme
  }

getBase :: (Moon m) => Moonbase m (Ref m)
getBase = ask

ioPushSignal :: (Moon m) => Runtime m -> Signal -> IO ()
ioPushSignal rt = atomically . writeTQueue (_signals rt)

pushSignal   :: (Moon m) => Ref m -> Signal -> m ()
pushSignal ref s = io $ flip ioPushSignal s =<< readTVarIO ref

readSignal   :: (Moon m) => Ref m -> m Signal
readSignal ref = io $ atomically $ readTQueue =<< _signals <$> readTVar ref
{-
instance (Moon m) => HasSignals (Runtime m) m where
  ioPushSignal state s = atomically $ writeTQueue (_signals state) s
  pushSignal s base = do
    state <- getBase base
    io $ ioPushSignal state s
  readSignal base = do
    queue <- (_signals state) <$> getBase base
    io $ atomically $ readTQueue queue -}




-- A terminal function. First argument is
type Terminal m = (Maybe String -> Moonbase m ())


-- | Generates a new moonbase runtime
--mkRuntime :: DBusClient -> Options -> Terminal -> IO (TQueue Signal, TVar Runtime)
--mkRuntime dbusClient opts term = do
--    sigs <- atomically newTQueue
--    runtime <- atomically $ newTVar (new sigs)
--    return (sigs, runtime)
--  where
--    new sigs = Runtime dbusClient opts Nothing sigs term M.empty defaultTheme

makeLenses ''Action
makeLenses ''Options
makeLenses ''Runtime
