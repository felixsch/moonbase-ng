{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Moonbase.Panel where

import Control.Lens
import Control.Applicative
import Control.Monad

import Moonbase.Core
import Moonbase.Theme
import Moonbase.Util
import Moonbase.Util.Gtk
import Moonbase.Util.Css

import Moonbase.Signal

import Control.Monad.STM
import Control.Concurrent.STM.TVar

import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp(..))

{-
  panel <- withPanel 20 Top (OnMonitor 1) (xmonad --> clock <-- cpu)
  mpdPanel <- withPanel 20 Bottom SpanMonitors (mdpCurrentSong <> mpdVolume)
-}

data PanelMode = OnMonitor Int
               | SpanMonitors
               | OnScreen Int PanelMode

instance Show PanelMode where
  show (OnMonitor num)  = "on-monitor-" ++ show num
  show SpanMonitors     = "span-monitor"
  show (OnScreen num c) = "on-screen-" ++ show num ++ "-" ++ show c


getMode :: Gtk.Display -> PanelMode -> IO (Gtk.Rectangle, Gtk.Screen)
getMode display mode = do
    screen <- Gtk.displayGetScreen display num

    rect   <- case mode' of
      OnMonitor x  -> Gtk.screenGetMonitorGeometry screen x
      SpanMonitors -> Gtk.Rectangle 0 0 <$> Gtk.screenGetWidth screen 
                                        <*> Gtk.screenGetHeight screen    
    return (rect, screen)
  where 
    (num, mode') = screenNum 0 mode

    screenNum _ (OnScreen x m) = screenNum x m
    screenNum n m              = (n, m)
 

data PanelConfig = PanelConfig
  { panelHeight   :: Int
  , panelPosition :: Position
  , panelMode     :: PanelMode
  , panelStyle    :: DefaultTheme }

data PanelItem = PanelItem 
  { _paneItemName     :: Name
  , _panelItemWidget  :: Gtk.Widget
  , _panelItemPacking :: Gtk.Packing }

makeLenses ''PanelItem

data PanelItems = PanelItems [Configure Panel PanelItem]

instance Monoid PanelItems where
    mempty  = PanelItems []
    mappend (PanelItems a) (PanelItems b) = PanelItems (a ++ b)

item :: Configure Panel PanelItem -> PanelItems
item gen = PanelItems [gen]


data PanelState = PanelState
  { panelItems  :: PanelItems
  , panelWindow :: Gtk.Window
  , panelHBox   :: Gtk.HBox }

type Panel = TVar PanelState


withPanel :: PanelConfig -> PanelItems -> Moon Panel
withPanel config items = do
  debug "a panel"

  withDisplay $ \display -> do
 
    (size, screen) <- liftIO $ getMode display (panelMode config)
    window         <- liftIO $ Gtk.windowNew

    liftIO $ do

      Gtk.widgetSetName window $ "panel-" ++ show (panelMode config)
                                          ++ "-"
                                          ++ show (panelPosition config)

      Gtk.windowSetScreen   window screen
      Gtk.windowSetTypeHint window Gtk.WindowTypeHintDock
      Gtk.windowSetGravity  window Gtk.GravityStatic

      Gtk.widgetSetCanFocus window False

      Gtk.set window [ Gtk.windowSkipTaskbarHint := True
                    , Gtk.windowSkipPagerHint   := True
                    , Gtk.windowAcceptFocus     := False
                    , Gtk.windowDecorated       := False
                    , Gtk.windowHasResizeGrip   := False
                    , Gtk.windowResizable       := False ]

      setPanelSize config size window

      _ <- Gtk.on screen Gtk.screenMonitorsChanged $
        setPanelSize config size window

      box <- Gtk.hBoxNew False 2
      Gtk.containerAdd window box

      withCss window $ do
        bgColor styleBgColor
        fgColor styleFgColor
        


      Gtk.widgetShowAll window

      atomically $ newTVar (PanelState items window box)

  where
      styleBgColor = bg $ getNormal $ panelStyle config
      styleFgColor = color $ getNormal $ panelStyle config

    
setPanelSize :: PanelConfig -> Gtk.Rectangle -> Gtk.Window -> IO ()
setPanelSize config geo@(Gtk.Rectangle x y w h) window = do

  Gtk.windowSetDefaultSize window w height
  Gtk.widgetSetSizeRequest window w height
  Gtk.windowResize         window w height

  moveWindow               window position geo
  setWindowHints           window geo

  _ <- Gtk.on window Gtk.realize $ setWindowStruts window position height geo

  isRealized <- Gtk.widgetGetRealized window

  when isRealized $ setWindowStruts window position height geo
    where
      height   = panelHeight config
      position = panelPosition config

      


{-
getPanelSizes :: Gtk.Display -> PanelMode -> Moon Gtk.Rectangle
getPanelSizes disp (OnScreen num conf)
  = getPanelSizes' conf =<< liftIO $ Gtk.displayGetScreen disp num
getPanelSizes disp conf
  = getPanelSizes' conf =<< liftIO $ Gtk.displayGetDefaultScreen disp

getPanelSizes' :: PanelMode -> Gtk.Screen -> Moon Gtk.Rectangle
getPanelSizes' SpanMonitors screen = liftIO $ do
  w <- Gtk.screenGetWidth screen
  h <- Gtk.screenGetHeight screen
  return $ Gtk.Rectangle 0 0 w h
getPanelSizes' (OnMonitor num) = liftIO $ Gtk.screenGetMonitorGeometry screen num
getPanelSizes' (OnScreen _ c)  = getPanelSizes' conf screen

-}












spaceRight :: Maybe String -> PanelItems
spaceRight mlabel = item $ do
  label <- liftIO $ Gtk.labelNew mlabel
  return $ PanelItem "spacer-right" (Gtk.toWidget label) Gtk.PackGrow

(-->) :: PanelItems -> PanelItems -> PanelItems
(PanelItems a) --> (PanelItems b) = PanelItems $ a ++ spacer ++ b
  where
    (PanelItems spacer) = spaceRight Nothing


spaceLeft :: Maybe String -> PanelItems
spaceLeft mlabel = item $ do
  label <- liftIO $ Gtk.labelNew mlabel
  return $ PanelItem "spacer-left" (Gtk.toWidget label) Gtk.PackGrow

(<--) :: PanelItems -> PanelItems -> PanelItems 
(PanelItems a) <-- (PanelItems b) = PanelItems $ a ++ spacer ++ b
  where
    (PanelItems spacer) = spaceLeft Nothing




  



