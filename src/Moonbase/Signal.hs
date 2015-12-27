module Moonbase.Signal where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad

import           Moonbase.Core


isDebug :: Bool
isDebug = True


verbose :: Moon () -> Moon ()
verbose f = do
  rt <- get
  when (rt ^. options ^. isVerbose) f

signal :: Signal -> Moon ()
signal sig = do
  s <- use signals
  liftIO $ atomically (writeTQueue s sig)

warn :: String -> Moon ()
warn msg = signal $ SignalMessage Moonbase.Core.Warning msg

say :: String -> Moon ()
say msg = signal $ SignalMessage Moonbase.Core.Info msg

success :: String -> Moon ()
success msg = signal $ SignalMessage Moonbase.Core.Success msg

fatal :: String -> Moon ()
fatal msg = signal $ SignalFatalError msg

debug :: String -> Moon ()
debug msg = when isDebug $ liftIO $ putStrLn $ "[DEBUG]: " ++ msg
