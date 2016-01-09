{-# LANGUAGE TemplateHaskell #-}
module Moonbase.Desktop where

import           Control.Lens
import           Control.Monad
import           Control.Monad.STM

import           Control.Concurrent.STM.TVar

import           Control.Monad.State

import qualified Graphics.UI.Gtk             as Gtk
import qualified System.Glib.GError          as Glib

import           Moonbase.Core
import           Moonbase.Signal
import           Moonbase.Theme              (Color)
import           Moonbase.Util
import           Moonbase.Util.Gtk

import           Data.Word


type MonitorNum = Int
data Monitor = Monitor
    { _monitorNum    :: MonitorNum
    , _monitorGeo    :: Gtk.Rectangle
    , _monitorWindow :: Gtk.Window }

makeLenses ''Monitor

type ScreenNum = Int
data Screen m = Screen
  { _screenNum        :: ScreenNum
  , _screenResolution :: (Int, Int)
  , _screenMonitors   :: [Monitor]
  , _screenComposited :: Bool
  , _screenScreen     :: Gtk.Screen
  , _screenConfigure  :: Configure (Screen m) m () }

makeLenses ''Screen


data DesktopState m = DesktopState
  { _desktopScreens :: [Screen m]
  , _desktopDisplay :: Gtk.Display }

type Desktop m = TVar (DesktopState m)

makeLenses ''DesktopState


initializeScreen :: (Moon m) => Gtk.Display -> ScreenNum -> Moonbase m (Screen m)
initializeScreen disp num = do
  debug $ "Initialize screen " ++ show num ++ "..."

  screen      <- io $ Gtk.displayGetScreen disp num

  numMonitors <- io $ Gtk.screenGetNMonitors screen
  w           <- io $ Gtk.screenGetWidth screen
  h           <- io $ Gtk.screenGetHeight screen
  composited  <- io $ Gtk.screenIsComposited screen

  debug $ "  :: " ++ show numMonitors ++ " assigned to this screen"
  monitors    <- mapM (initializeMonitor screen) [0..(numMonitors - 1)]

  return $ Screen num (w,h) monitors composited screen (return ())


initializeMonitor :: (Moon m) => Gtk.Screen -> MonitorNum -> Moonbase m Monitor
initializeMonitor screen num = do
  debug $ "Initialize monitor " ++ show num ++ "..."

  rect   <- io $ Gtk.screenGetMonitorGeometry screen num
  window <- io $ newMonitorWindow num screen rect

  return $ Monitor num rect window


withDesktop :: (Moon m) => Configure (DesktopState m) (Moonbase m) () -> Moonbase m (Desktop m)
withDesktop conf = do
  debug "with basic Desktop..."

  disp       <- checkDisplay =<< io Gtk.displayGetDefault
  numScreens <- io $ Gtk.displayGetNScreens disp

  debug $ "  :: " ++ show numScreens ++ " screens available"
  screens    <- mapM (initializeScreen disp) [0..(numScreens - 1)]

  let state = DesktopState screens disp

  state' <- configure state conf

  showDesktops state'

  io $ atomically $ newTVar state'

    -- TODO:
    -- Add displayClosed handling
    -- Add screenChanged handling
    -- Add compositChanged handling




showDesktops :: (Moon m) => (DesktopState m) -> Moonbase m ()
showDesktops state = do
  let windows  = state ^.. allMonitors . traverse . monitorWindow

  forM_ windows $ \w -> io $ do
    Gtk.widgetQueueDraw w
    Gtk.widgetShowAll w
      where
    allMonitors = desktopScreens . traverse . screenMonitors


onEveryScreen :: (Moon m) => Configure (Screen m) m () -> Configure (DesktopState m) m ()
onEveryScreen conf = do
  lift $ debug "running on every screen"

  desktopScreens . traverse . screenConfigure .= conf

  screens <- use desktopScreens

  screens' <- mapM configure' screens

  desktopScreens .= screens'

  where
    configure' :: (Moon m) => Screen m -> Configure (DesktopState m) m (Screen m)
    configure' s = lift $ configure s conf


onEveryMonitor :: (Moon m) => Configure Monitor m () -> Configure (DesktopState m) m ()
onEveryMonitor conf = onEveryScreen $ do
  lift $ debug "running on every monitor..."

  monitors <- use screenMonitors

  monitors' <- mapM configure' monitors

  screenMonitors .= monitors'

  where
    configure' m = lift $ configure m conf


setBackgroundColor :: (Moon m) => Color -> Configure Monitor m ()
setBackgroundColor c = do
  window                   <- use monitorWindow
  num                      <- use monitorNum
  (Gtk.Rectangle x y w h)  <- use monitorGeo

  lift $ debug $ "Setting Color of Monitor " ++ show num ++ " to " ++ c

  image <- io $ do
    buf   <- Gtk.pixbufNew Gtk.ColorspaceRgb False 8 w h
    Gtk.pixbufFill buf r g b 255
    Gtk.imageNewFromPixbuf buf

  io $ Gtk.containerAdd window image
    where
    (r,g,b,_) = parseColorTuple c :: (Word8, Word8, Word8, Word8)


setWallpaper :: (Moon m) => FilePath -> Configure Monitor m ()
setWallpaper path = do
  window                  <- use monitorWindow
  num                     <- use monitorNum
  (Gtk.Rectangle x y w h) <- use monitorGeo

  lift $ debug $ "Setting Wallpaper of Monitor " ++ show num ++ " to " ++ path

  image <- io $ do
    pixbuf <- Glib.catchGErrorJustDomain (wallpaper w h) (errorPixBuf w h)
    Gtk.imageNewFromPixbuf pixbuf

  io $ Gtk.containerAdd window image

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

setComposited :: (Moon m) => Gtk.Display -> jkScreen m -> Moonbase m (Screen m)
setComposited disp screen = do
   screen'     <- io $ Gtk.displayGetScreen disp $ _screenNum screen
   composited  <- io $ Gtk.screenIsComposited screen'
   return $ screen { _screenComposited = composited }
