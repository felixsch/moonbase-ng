{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
--{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Moonbase.Core
 ( Exception(..)
 , Moon(..)
 , Base(..)
 , Moonbase(..)
 , moon
 , eval
 , Message(..)

 , DBusClient
 , Argument

 , Action(..)
 , actionName, actionHelp, action
 , ActionError(..)
 , ActionResult(..)
 , actionResult
 , actionError
 , actionNothing

 , Runtime(..)
 , hdl, actions, theme, dbus, isVerbose

 -- re-imports
 , get, put, modify
 , ask
 , E.throw
 ) where
{-
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
-}

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
import qualified DBus
import qualified DBus.Client                         as DBus
import           System.Environment.XDG.DesktopEntry
import           System.IO
import qualified System.Timeout                      as T

import           Moonbase.Theme

-- Core Types ------------------------------------------------------------------

type DBusClient = DBus.Client

data Exception = CouldNotOpenDisplay
               | DBusError String
               | FileNotFound FilePath
               | Shutdown
               deriving (Show)

instance E.Exception Exception

data Message = Warning    -- ^ A warning
             | Info       -- ^ An information
             | Success    -- ^ Successfully exectued action
             | Debug      -- ^ Debuging output
             deriving (Show, Eq, Enum)


class Monad m => Moon m where
  io      :: IO a -> m a
  puts    :: String -> m ()
  content :: FilePath -> m String
  fork    :: m () -> m ThreadId
  timeout :: Int -> m a  -> m (Maybe a)

class Base rt where
  data BaseRef rt :: *
  newBase :: (Moon m) => rt -> m (BaseRef rt)
  base :: (Moon m) => BaseRef rt -> m rt
  update :: (Moon m) => BaseRef rt -> rt -> m ()

  logB  :: (Moon m) => BaseRef rt -> Message -> m ()
  dbusB :: (Moon m) => BaseRef rt -> m DBusClient

  addActionB :: (Moon m) => BaseRef rt -> String -> Action rt m -> Moonbase st m ()
  allActionsB :: (Moon m) => BaseRef rt -> Moonbase st m [Action rt m]



-- Actions ---------------------------------------------------------------------

type Argument = String
data ActionError = ActionNotFound
                  | ActionFileNotFound
                  | ActionError String
                  deriving (Show)

type ActionResult = Either ActionError (Maybe String)

actionError :: ActionError -> ActionResult
actionError = Left

actionResult :: (Show a) => a -> ActionResult
actionResult = Right . Just . show

actionNothing :: ActionResult
actionNothing = Right Nothing

data Action rt m = Action
  { _actionName :: String
  , _actionHelp :: String
  , _action     :: [Argument] -> Moonbase rt m ActionResult }


-- Runtime ---------------------------------------------------------------------

data Runtime m = Runtime
 { _hdl       :: Handle
 , _actions   :: [Action (Runtime m) m]
 , _theme     :: Theme
 , _isVerbose :: Bool
 , _dbus      :: DBusClient }


-- Moonbase --------------------------------------------------------------------

newtype (Base rt) => Moonbase rt m a = Moonbase (ReaderT (BaseRef rt) m a)
  deriving (Functor, Monad, MonadReader (BaseRef rt))

moon :: (Moon m, Base rt) => m a -> Moonbase rt m a
moon = Moonbase . lift

instance (Monad m) => Applicative (Moonbase rt m) where
  pure  = return
  (<*>) = ap

instance (Moon m, Base rt) => MonadState rt (Moonbase rt m) where
  get = base =<< ask
  put rt = do
    ref <- ask
    update ref rt

instance (Base rt, Moon m) => Moon (Moonbase rt m) where
  io      = moon . io
  puts    = moon . puts
  content = moon . content
  fork f  = do
    ref <- ask
    moon $ fork (void $ eval ref f)
  timeout s f = do
    ref <- ask
    moon $ timeout s (eval ref f)


eval :: (Base rt, Moon m) => BaseRef rt -> Moonbase rt m a -> m a
eval ref (Moonbase f) = runReaderT f ref

makeLenses ''Action
makeLenses ''Runtime




  {-
dbusPPOutput :: DBusClient -> String -> IO ()
dbusPPOutput client str = DBus.emit client signal'
    where
        signal'   = (DBus.signal path interface member) { DBus.signalBody = [body] }
        path      = DBus.objectPath_ "/org/moonbase/XMonadLog"
        interface = DBus.interfaceName_ "org.moonbase.XMonadLog"
        member    = DBus.memberName_ "Update"
        body      = DBus.toVariant $ Utf8.decodeString str
  xon       ::  (XMoon m) => ([Argument] -> XMoonbase rt m XActionResult) -> XMoonbase rt m ()
  xcallback ::  (XMoon m) => (Signal -> XMoonbase rt m ()) -> XMoonbase rt m ()
-}

{-

  CLI
[]   - action ausfuehren
[]   - moonbase starten
[]   - moonbase stoppen (via action)

  SESSION
[]   - logging
[]   - actions
[]   - power management
[]   - xrandr / udev lid closed
[]   - autostart
[]   + on docking station


  FUNCTIONS
[]    - wm
[]      - xmonad
[]      - moonbase
[]      + xcompmgr
[]    - notify daemon
[]    - panel
[]      - panel items
[]        - clock
[]        - expander
[]        - dbus
[]           - generic log
[]           - xmonad
[]           - messages
[]        - shell
[]           - plain
[]           - simple syntax
        - bar vert / horiz
          - cpu
          - mem
          - net
          - disk
        - systemtray
        - power buttons
-}


{-
type Name = String


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
  content :: FilePath -> m String
  fork    :: m () -> m ThreadId
  timeout :: Int -> m a  -> m (Maybe a)

instance Moon IO where
  io      = id
  puts    = putStrLn
  content = readFile
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
  content = liftMoon . content
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
  content = liftConfigure . content
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
makeLenses ''Runtime -}
