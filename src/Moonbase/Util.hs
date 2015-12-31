
module Moonbase.Util
  ( Position(..)
  , parseColorTuple, clamp, hex
  , lift, liftIO
  , pollForever, pollForever_
  ) where

import           Control.Concurrent  (forkIO, threadDelay)
import           Control.Monad       (forever)
import           Control.Monad.State
import           Numeric             (readHex)

import           Data.Char           (isHexDigit)

import           Moonbase.Core
import           Moonbase.Theme      (Color, defaultColor)

data Position = Top
              | Bottom
              | Custom Int

instance Show Position where
    show Top          = "top"
    show Bottom       = "bottom"
    show (Custom num) = "offset-" ++ show num


hex :: (Eq a, Num a) => String -> a
hex = fst . head . readHex

parseColorTuple :: (Num a, Eq a) => Color -> (a, a, a, a)
parseColorTuple   ['#',r,g,b]   = parseColorTuple ['#', r, r, g, g, b, b, 'f', 'f']
parseColorTuple   ['#',r,g,b,a] = parseColorTuple ['#', r, r, g, g, b, b, a, a]
parseColorTuple   ['#', r1, r2, g1, g2, b1, b2] =
    parseColorTuple ['#', r1, r2, g1, g2, b1, b2, 'f', 'f']
parseColorTuple c@['#', r1, r2, g1, g2, b1, b2, a1, a2]
  | all isHexDigit (tail c) = ( hex [r1,r2],
                                hex [g1,g2],
                                hex [b1,b2],
                                hex [a1,a2] )
  | otherwise               = parseColorTuple defaultColor
parseColorTuple _           = parseColorTuple defaultColor


clamp :: (Integral a) => (a,a,a,a) -> (Double, Double, Double, Double)
clamp (r,g,b,a) = (cl r, cl g, cl b, cl a)
  where
    cl x = fromIntegral x / 255.0

pollForever_ :: (Moon m) => Int -> m () -> m ()
pollForever_ ms f = void $ fork $ forever $ f >> io (threadDelay (ms * 1000))

pollForever :: (Moon m) => Int -> a -> (a -> m a) -> m ()
pollForever ms i f = void $ fork $ void $ loop i
  where
    loop a = do
      a' <- f a
      io $ threadDelay (ms * 1000)
      loop a'
