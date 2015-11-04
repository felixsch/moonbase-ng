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


import DBus
import DBus.Client

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

data PanelItems = PanelItems [Configure PanelState PanelItem]

instance Monoid PanelItems where
    mempty  = PanelItems []
    mappend (PanelItems a) (PanelItems b) = PanelItems (a ++ b)

item :: Configure PanelState PanelItem -> PanelItems
item gen = PanelItems [gen]


data PanelState = PanelState
  { _panelItems  :: [PanelItem]
  , _panelWindow :: Gtk.Window
  , _panelHBox   :: Gtk.HBox 
  , _panelConfig :: PanelConfig }

makeLenses ''PanelState

type Panel = TVar PanelState


withPanel :: PanelConfig -> PanelItems -> Moon Panel
withPanel config (PanelItems items) = do
  debug "a panel"

  withDisplay $ \display -> do
 
    (size, screen) <- liftIO $ getMode display (panelMode config)
    window         <- liftIO Gtk.windowNew

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

      _ <- Gtk.on screen Gtk.screenMonitorsChanged $
        setPanelSize config size window

      setPanelSize config size window

    box <- liftIO $ Gtk.hBoxNew False 2

    withCss window $ do
      bgColor styleBgColor
      fgColor styleFgColor

    (items, panel) <- configureWith (PanelState [] window box config) $
      sequence items

    debug "Added panel"
    forM_ items $ \(PanelItem n w p) -> do
      debug $ "  - item: " ++ n
      liftIO $ Gtk.boxPackStart box w p 0
      

  
    liftIO $ do
      Gtk.containerAdd window box
      Gtk.widgetShowAll window
      atomically $ newTVar $ panel { _panelItems = items }

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

      
dbusLabel :: String -> MatchRule -> PanelItems
dbusLabel name match = item $ do
        client  <- lift $ use dbus
        label   <- liftIO $ Gtk.labelNew (Just "waiting for xmonad...")
        lift $ debug "Added xmonad logger"
        liftIO $ addMatch client match $ \signal -> do
            let Just str = fromVariant $ head (signalBody signal) :: Maybe String
            putStrLn $ "new message: " ++ str
            Gtk.postGUISync $ Gtk.labelSetMarkup label str

        return $ PanelItem name (Gtk.toWidget label) Gtk.PackNatural


xmonadLog :: PanelItems
xmonadLog = dbusLabel "xmonadLog" rule
    where
        rule      = matchAny 
          { matchPath      = Just path
          , matchInterface = Just interface
          , matchMember    = Just member }
        path      = DBus.objectPath_ "/org/moonbase/XMonadLog"
        interface = DBus.interfaceName_ "org.moonbase.XMonadLog"
        member    = DBus.memberName_ "Update"



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











{-
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
-}




  



