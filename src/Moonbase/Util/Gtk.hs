{-|
Module      : Moonbase.Theme
Copyright   : (c) Felix Schnizlein, 2014
License     : GPL-2
Maintainer  : felix@none.io
Stability   : experimental
Portability : POSIX

Some helper function to complete Gtk's functionality

-}

module Moonbase.Util.Gtk 
 ( iosync
 , ioasync
 , withDisplay
 , pangoColor
 , pangoSanitize
 , Position(..)
 , moveWindow
 , setWindowHints
 , setWindowStruts
 , getAbsoluteMousePosition
 , parseColorGtk
 , parseColorTuple
 , clamp
 , setStyle
 ) where

import Control.Monad.Reader
import Control.Applicative

import Numeric (readHex)

import Data.Char (isHexDigit)

import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.General.StyleContext as Gtk
import qualified Graphics.UI.Gtk.General.CssProvider as Gtk

import Moonbase.Core
import Moonbase.Signal
import Moonbase.Theme
import Moonbase.Util.StrutProperties


-- | Window positions
data Position = Top -- ^ At top
  | Bottom -- ^ At bottom
  | Custom Int -- ^ At a custom position


withDisplay :: (Gtk.Display -> Moon a) -> Moon (Maybe a)
withDisplay f = do
    disp <- liftIO $ Gtk.displayGetDefault
    case disp of
         Just d  -> Just <$> f d
         Nothing -> fatal "Could not open display!" >> return Nothing


-- | Wrapper arroung liftIO . Gtk.postGUISync
iosync :: (MonadIO m) => IO a -> m a
iosync = liftIO . Gtk.postGUISync

ioasync :: (MonadIO m) => IO () -> m ()
ioasync = liftIO . Gtk.postGUIAsync

-- | Applys pango color formatting to a 'String'
pangoColor :: String -> String -> String
pangoColor fg str = left ++ str ++ right
    where
        left = "<span foreground=\"" ++ color_ fg ++ "\">"
        right = "</span>" 

-- | Sanatize few basic characters
pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>' xs = "&gt;" ++ xs
    sanitize '<' xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&' xs = "&amp;" ++ xs
    sanitize x xs = x:xs

-- | Move Window to given position
moveWindow :: Gtk.Window     -- ^ Window which should be moved
           -> Position       -- ^ Position where the window should moved
           -> Gtk.Rectangle  -- ^ Size of the monitor
           -> IO ()
moveWindow win pos (Gtk.Rectangle x _ _ h) = do
    (_, height) <- Gtk.windowGetSize win 
    Gtk.windowMove win x (offset height)
    where
        offset height = case pos of
                      Top            -> 0
                      Bottom         -> h - height
                      Custom height' -> h - height - height'

-- | Set window geometry hints (a easy wrapper for full horizontal windows)
setWindowHints :: Gtk.Window    -- ^ Window where geometry hints should set
               -> Gtk.Rectangle -- ^ Size of the monitor where the window is on
               -> IO ()
setWindowHints win (Gtk.Rectangle _ _ w _) = do
    (_, h) <- Gtk.windowGetSize win
    Gtk.windowSetGeometryHints win noWidget (Just (w,h)) (Just (w,h)) Nothing Nothing Nothing
      where
          noWidget = Nothing :: Maybe Gtk.Widget

-- | Generate strutProperties for fully horizontal windows
strutProperties :: Position        -- ^ Window position
                -> Int             -- ^ Window height
                -> Gtk.Rectangle   -- ^ Current monitor rectangle
                -> [Gtk.Rectangle] -- ^ All monitors
                -> StrutProperties
strutProperties pos bh (Gtk.Rectangle mX mY mW mH) monitors = propertize pos sX sW sH
    where
        sX = mX
        sW = mW - 1
        sH = case pos of
            Top -> bh + mY
            Bottom -> bh + totalH - mY - mH
        totalH = maximum $ map bottomY monitors
        bottomY (Gtk.Rectangle _ y _ h) = y + h
        propertize p x w h = case p of
            Top    -> StrutProperties 0 0 h 0 0 0 0 0 x (x+w) 0 0
            Bottom -> StrutProperties 0 0 0 h 0 0 0 0 0 0 x (x+w)

-- | Sets window struts
setWindowStruts :: Gtk.Window -> Position -> Int -> Gtk.Rectangle -> IO ()
setWindowStruts win pos height geo = do
    scr    <- Gtk.windowGetScreen win
    moNum  <- Gtk.screenGetNMonitors scr
    moGeos <- mapM (Gtk.screenGetMonitorGeometry scr) [0 .. (moNum - 1)]
    
    setStrutProperties win $ strutProperties pos height geo moGeos

-- | Returns the absolute mouse position
-- 
-- If the mouse pointer is not on the screen (which is usual the case with Xinerama and nvidia twinview)
-- this function return (0,0)
getAbsoluteMousePosition :: Gtk.Screen -> IO (Int, Int)
getAbsoluteMousePosition scr = do
    root <- Gtk.screenGetRootWindow scr
    mPos <- Gtk.drawWindowGetPointer root
    return $ check mPos
      where
          check (Just (True, x, y, _)) = (x,y)
          check _                      = (0,0)



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


parseColorGtk :: Color -> Gtk.Color
parseColorGtk c = Gtk.Color (imp r) (imp g) (imp b)
  where
    imp       = (*) 257
    (r,g,b,_) = parseColorTuple c

clamp :: (Eq a, Num a, Integral a, Fractional a) => (a,a,a,a) -> (Double, Double, Double, Double)
clamp (r,g,b,a) = (cl a, cl b, cl g, cl a)
  where
    cl x = fromIntegral $ x / 255
          


setStyle :: (Gtk.WidgetClass widget) =>  widget -> String -> [(String, String)] -> IO ()
setStyle w name settings = do
    Gtk.widgetSetName w name

    provider <- Gtk.cssProviderNew 
    context  <- Gtk.widgetGetStyleContext w

    Gtk.cssProviderLoadFromString provider css

    Gtk.styleContextAddProvider context provider 800
 where
    parsedList ((k, p) : xs) = (k ++ ": " ++ p ++ ";") : parsedList xs
    parsedList []          = []

    css = "#" ++ name ++ " {"
        ++ unwords (parsedList settings) 
        ++ "}"

