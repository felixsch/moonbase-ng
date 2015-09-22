{-# LANGUAGE TemplateHaskell #-}
module Moonbase.Desktop where

import Control.Lens
import Control.Monad
import Control.Monad.STM

import Control.Concurrent.STM.TVar

import Control.Monad.State

import qualified Graphics.UI.Gtk as Gtk
import qualified System.Glib.GError as Glib

import Moonbase.Core
import Moonbase.Util
import Moonbase.Signal
import Moonbase.Theme (Color)
import Moonbase.Util.Gtk

import Data.Word


type MonitorNum = Int
data Monitor = Monitor
    { _monitorNum        :: MonitorNum
    , _monitorGeo        :: Gtk.Rectangle
    , _monitorWindow     :: Gtk.Window }

makeLenses ''Monitor

type ScreenNum = Int
data Screen = Screen
  { _screenNum        :: ScreenNum
  , _screenResolution :: (Int, Int)
  , _screenMonitors   :: [Monitor]
  , _screenComposited :: Bool
  , _screenScreen     :: Gtk.Screen
  , _screenConfigure  :: Configure Screen () }

makeLenses ''Screen


data DesktopState = DesktopState
  { _desktopScreens   :: [Screen]
  , _desktopDisplay   :: Gtk.Display }

type Desktop = TVar DesktopState

makeLenses ''DesktopState


initializeScreen :: Gtk.Display -> ScreenNum -> Moon Screen
initializeScreen disp num = do
  debug $ "Initialize screen " ++ show num ++ "..."

  screen      <- liftIO $ Gtk.displayGetScreen disp num

  numMonitors <- liftIO $ Gtk.screenGetNMonitors screen
  w           <- liftIO $ Gtk.screenGetWidth screen
  h           <- liftIO $ Gtk.screenGetHeight screen
  composited  <- liftIO $ Gtk.screenIsComposited screen

  debug $ "  :: " ++ show numMonitors ++ " assigned to this screen"
  monitors    <- mapM (initializeMonitor screen) [0..(numMonitors - 1)]

  return $ Screen num (w,h) monitors composited screen (return ())


initializeMonitor :: Gtk.Screen -> MonitorNum -> Moon Monitor
initializeMonitor screen num = do
  debug $ "Initialize monitor " ++ show num ++ "..."

  rect   <- liftIO $ Gtk.screenGetMonitorGeometry screen num
  window <- liftIO $ newMonitorWindow num screen rect

  return $ Monitor num rect window


withDesktop :: Configure DesktopState () -> Moon Desktop
withDesktop conf = do
  debug $ "with basic Desktop..."

  disp       <- checkDisplay =<< liftIO Gtk.displayGetDefault
  numScreens <- liftIO $ Gtk.displayGetNScreens disp

  debug $ "  :: " ++ show numScreens ++ " screens available"
  screens    <- mapM (initializeScreen disp) [0..(numScreens - 1)]

  let state = DesktopState screens disp

  state' <- configure state conf

  showDesktops state'

  desktop <- liftIO $ atomically $ newTVar state'

    -- TODO:
    -- Add displayClosed handling
    -- Add screenChanged handling
    -- Add compositChanged handling

  return desktop



showDesktops :: DesktopState -> Moon ()
showDesktops state = do
  let windows  = state ^.. allMonitors . traverse . monitorWindow

  forM_ windows $ \w -> liftIO $ do
    Gtk.widgetQueueDraw w
    Gtk.widgetShowAll w
      where
    allMonitors = desktopScreens . traverse . screenMonitors


onEveryScreen :: Configure Screen () -> Configure DesktopState ()
onEveryScreen conf = do
  lift $ debug "running on every screen"

  desktopScreens . traverse . screenConfigure .= conf

  screens <- use desktopScreens

  screens' <- mapM configure' screens

  desktopScreens .= screens'

  where
    configure' s = lift $ configure s conf


onEveryMonitor :: Configure Monitor () -> Configure DesktopState ()
onEveryMonitor conf = onEveryScreen $ do
  lift $ debug "running on every monitor..." 

  monitors <- use screenMonitors

  monitors' <- mapM configure' monitors

  screenMonitors .= monitors'

  where
    configure' m = lift $ configure m conf

  
setBackgroundColor :: Color -> Configure Monitor ()
setBackgroundColor c = do
  window                   <- use monitorWindow
  num                      <- use monitorNum
  (Gtk.Rectangle x y w h)  <- use monitorGeo

  lift $ debug $ "Setting Color of Monitor " ++ show num ++ " to " ++ c
 
  image <- liftIO $ do
    buf   <- Gtk.pixbufNew Gtk.ColorspaceRgb False 8 w h
    Gtk.pixbufFill buf r g b 255
    Gtk.imageNewFromPixbuf buf

  liftIO $ Gtk.containerAdd window image
    where 
    (r,g,b,_) = parseColorTuple c :: (Word8, Word8, Word8, Word8)


setWallpaper :: FilePath -> Configure Monitor ()
setWallpaper path = do
  window                  <- use monitorWindow
  num                     <- use monitorNum
  (Gtk.Rectangle x y w h) <- use monitorGeo

  lift $ debug $ "Setting Wallpaper of Monitor " ++ show num ++ " to " ++ path

  image <- liftIO $ do
    pixbuf <- Glib.catchGErrorJustDomain (wallpaper w h) (errorPixBuf w h)
    Gtk.imageNewFromPixbuf pixbuf

  liftIO $ Gtk.containerAdd window image

  where
    wallpaper w h       = Gtk.pixbufNewFromFileAtScale path w h False
    errorPixBuf :: Int -> Int -> Gtk.PixbufError -> Glib.GErrorMessage -> IO Gtk.Pixbuf
    errorPixBuf w h _ _ = do
       buf <- Gtk.pixbufNew Gtk.ColorspaceRgb False 8 w h
       Gtk.pixbufFill buf 0 0 0 255
       return buf


newMonitorWindow :: Int -> Gtk.Screen -> Gtk.Rectangle -> IO Gtk.Window
newMonitorWindow i screen (Gtk.Rectangle x y w h) = do
    window <- Gtk.windowNew

    Gtk.windowSetScreen window screen

    Gtk.widgetSetName window ("desktop-" ++ show i)

    Gtk.windowSetGeometryHints window noWidget size size Nothing Nothing Nothing
    Gtk.windowMove window x y

    Gtk.windowSetDefaultSize window w h
    Gtk.widgetSetSizeRequest window w h
    Gtk.windowResize window w h
    
    Gtk.windowSetTypeHint window Gtk.WindowTypeHintDesktop
    Gtk.windowSetGravity  window Gtk.GravityStatic
    Gtk.widgetSetCanFocus window False

    return window
  where
    noWidget :: Maybe Gtk.Widget
    noWidget = Nothing 

    size :: Maybe (Int, Int)
    size = Just (w, h)
    
setComposited :: Gtk.Display -> Screen -> Moon Screen
setComposited disp screen = do
   screen'     <- liftIO $ Gtk.displayGetScreen disp $ _screenNum screen
   composited  <- liftIO $ Gtk.screenIsComposited screen'
   return $ screen { _screenComposited = composited }
