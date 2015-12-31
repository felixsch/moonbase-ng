module Moonbase.Pipe
  ( Pipe(..)
  , mkPipe
  , push
  , loopP, foreverP
  ) where


import           Control.Applicative
import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan
import           Control.Monad                (when)

import           Moonbase.Core


data Pipe a = Pipe (TChan a)

mkPipe :: (Moon m) => Moonbase m (Pipe a)
mkPipe = Pipe <$> io newBroadcastTChanIO

push :: (Moon m) => Pipe a -> a -> Moonbase m ()
push (Pipe chan) = io . atomically . writeTChan chan

loopP :: (Moon m) => Pipe a -> (a -> Moonbase m Bool) -> Moonbase m ()
loopP (Pipe broadcast) f = do
    chan <- io $ atomically (dupTChan broadcast)
    loop chan
  where
    loop chan = do
      value <- io $ atomically (readTChan chan)
      next <- f value
      when next $ loop chan

foreverP :: (Moon m) => Pipe a -> (a -> Moonbase m ()) -> Moonbase m ()
foreverP pipe f = loopP pipe (\a -> f a >> return True)
