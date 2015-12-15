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
 , moveWindow
 , widgetGetSize
 , setWindowHints
 , setWindowStruts
 , getAbsoluteMousePosition
 , parseColorGtk
 , parseColorTuple
 , clamp
 , setStyle
 , checkDisplay
 ) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad.Reader

import qualified Graphics.UI.Gtk                      as Gtk
import qualified Graphics.UI.Gtk.General.CssProvider  as Gtk
import qualified Graphics.UI.Gtk.General.StyleContext as Gtk

import           Moonbase.Core
import           Moonbase.Signal
import           Moonbase.Theme
import           Moonbase.Util
import           Moonbase.Util.StrutProperties


withDisplay :: (Gtk.Display -> Moon a) -> Moon a
withDisplay f = do
    disp <- liftIO Gtk.displayGetDefault
    case disp of
         Just d  -> f d
         Nothing -> do
           fatal "Could not open display!"
           liftIO $ throw CouldNotOpenDisplay


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

parseColorGtk :: Color -> Gtk.Color
parseColorGtk c = Gtk.Color (imp r) (imp g) (imp b)
  where
    imp       = (*) 257
    (r,g,b,_) = parseColorTuple c


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


checkDisplay :: Maybe Gtk.Display -> Moon Gtk.Display
checkDisplay Nothing     = fatal "Could not open display" >> error "Could not open display"
checkDisplay (Just disp) = return disp

widgetGetSize :: (Gtk.WidgetClass o, MonadIO m, Num a) => o -> m (a, a)
widgetGetSize chart = do
  area <- liftIO $ Gtk.widgetGetWindow chart
  case area of
       Nothing  -> return (0,0)
       Just win -> do
          w <- liftIO $ Gtk.drawWindowGetWidth win
          h <- liftIO $ Gtk.drawWindowGetHeight win
          return (fromIntegral w, fromIntegral h)
