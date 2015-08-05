module Moonbase.Pipe
  ( Pipe(..)
  , mkPipe
  , push
  , loopP, foreverP 
  ) where


import Control.Monad                (when)
import Control.Concurrent.STM       (atomically)
import Control.Concurrent.STM.TChan

import Moonbase.Core


data Pipe a = Pipe (TChan a)

mkPipe :: Moon (Pipe a)
mkPipe = Pipe <$> liftIO newBroadcastTChanIO

push :: Pipe a -> a -> Moon ()
push (Pipe chan) = liftIO . atomically . writeTChan chan

loopP :: Pipe a -> (a -> Moon Bool) -> Moon ()
loopP (Pipe broadcast) f = do
    chan <- liftIO $ atomically (dupTChan broadcast)
    loop chan
  where
    loop chan = do
      value <- liftIO $ atomically (readTChan chan)
      next <- f value
      when next $ loop chan

foreverP :: Pipe a -> (a -> Moon ()) -> Moon ()
foreverP pipe f = loopP pipe (\a -> f a >> return True)


  

