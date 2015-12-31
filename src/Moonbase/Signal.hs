module Moonbase.Signal where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad

import           Moonbase.Core


isDebug :: Bool
isDebug = True

verbose :: (Moon m) => Moonbase m () -> Moonbase m ()
verbose f = do
  rt <- get
  when (rt ^. options ^. isVerbose) f

signal :: (Moon m) => Signal -> Moonbase m ()
signal sig = do
  ref <- ask
  liftMoon $ pushSignal ref sig

warn :: (Moon m) => String -> Moonbase m ()
warn msg = signal $ SignalMessage Moonbase.Core.Warning msg

say :: (Moon m) => String -> Moonbase m ()
say msg = signal $ SignalMessage Moonbase.Core.Info msg

success :: (Moon m) => String -> Moonbase m ()
success msg = signal $ SignalMessage Moonbase.Core.Success msg

debug :: (Moon m) => String -> Moonbase m ()
debug msg = when isDebug $ puts $ "[DEBUG]: " ++ msg
