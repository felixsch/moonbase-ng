{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Moonbase.Panel where

import Control.Lens
import Control.Applicative
import Control.Monad

import Moonbase.Core
import Moonbase.Util
import Moonbase.Signal

import Control.Monad.STM
import Control.Concurrent.STM.TVar

import qualified Graphics.UI.Gtk as Gtk

{-
  panel <- withPanel Top (OnMonitor 1) (xmonad --> clock <-- cpu)
  mpdPanel <- withPanel Bottom SpanMonitors (mdpCurrentSong <> mpdVolume)
-}


data PanelItem = PanelItem 
  { _paneItemName     :: Name
  , _panelItemWidget  :: Gtk.Widget
  , _panelItemPacking :: Gtk.Packing }

makeLenses ''PanelItem

data PanelItems = PanelItems [Configure Panel PanelItem]

item :: Configure Panel PanelItem -> PanelItems
item gen = PanelItems [gen]


data PanelState = PanelState
  { panelItems  :: PanelItems
  , panelWindow :: Gtk.Window
  , panelHBox   :: Gtk.HBox }

type Panel = TVar PanelState

data PanelMode = OnMonitor Int
               | SpanMonitors
               | OnScreen Int PanelMode

instance Show PanelMode where
  show (OnMonitor num)  = "on-monitor-" ++ show num
  show SpanMonitors     = "span-monitor"
  show (OnScreen num c) = "on-screen-" ++ show num ++ "-" ++ show c

instance Monoid PanelItems where
    mempty  = PanelItems []
    mappend (PanelItems a) (PanelItems b) = PanelItems (a ++ b)


withPanel :: Position -> Int -> PanelMode -> PanelItems -> Moon Panel
withPanel pos width mode items = do
  debug $ "a panel"
  
  disp          <- checkDisplay =<< liftIO $ Gtk.displayGetDefault

  (Gtk.Rectangle x y w h)  <- getPanelSizes mode
  window                   <- Gtk.windowNew

  Gtk.windowSetScreen window screen
  Gtk.windowSetName $ "panel-" ++ show mode ++ "-" ++ show pos

  Gtk.windowSetTypeHint window Gtk.WindowTypeHintDock
  Gtk.windowSetGravity window Gtk.GravityStatic

  Gtk.widgetSetCanFocus window False
  Gtk.widgetModifyBg 





     -- FIXME: Add monitor support not the whole screen!
     scr       <- Gtk.displayGetScreen disp $ panelOnMonitor config
     screenNum <- Gtk.displayGetNScreens disp

     win       <- Gtk.windowNew

     Gtk.widgetSetName win "panel"

     Gtk.windowSetScreen   win scr
     Gtk.windowSetTypeHint win Gtk.WindowTypeHintDock 
     Gtk.windowSetGravity  win Gtk.GravityStatic

     
     Gtk.widgetSetCanFocus win False
     Gtk.widgetModifyBg    win Gtk.StateNormal (parseColor $ panelBg config)
     Gtk.widgetModifyFg    win Gtk.StateNormal (parseColor $ panelFg config)

     Gtk.set win [ Gtk.windowSkipTaskbarHint Gtk.:= True
                 , Gtk.windowSkipPagerHint Gtk.:= True
                 , Gtk.windowAcceptFocus Gtk.:= False
                 , Gtk.windowDecorated Gtk.:= False
                 , Gtk.windowHasResizeGrip Gtk.:= False
                 , Gtk.windowResizable Gtk.:= False ]

     setPanelSize config win

     _ <- Gtk.on scr Gtk.screenMonitorsChanged $ setPanelSize config win

     box <- Gtk.hBoxNew False 2
     Gtk.containerAdd win box

     return (win, box)

  



  debug $ "with basic Desktop..."

  disp       <- checkDisplay =<< liftIO Gtk.displayGetDefault
  numScreens <- liftIO $ Gtk.displayGetNScreens disp

  debug $ "  :: " ++ show numScreens ++ " screens available"
  screens    <- mapM (initializeScreen disp) [0..(numScreens - 1)]

  let state = DesktopState screens disp

  state' <- configure state conf

  showDesktops state'

  desktop <- liftIO $ atomically $ newTVar state'

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




  



