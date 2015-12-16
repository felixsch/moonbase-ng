{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
module Moonbase.Panel where

import           Control.Applicative
import           Control.Lens
import           Control.Monad

import           Moonbase.Core
import           Moonbase.Theme
import           Moonbase.Util
import           Moonbase.Util.Css
import           Moonbase.Util.Gtk

import           Moonbase.DBus
import           Moonbase.Signal

import           Control.Concurrent.STM.TVar
import           Control.Monad.STM

import           Graphics.UI.Gtk             (AttrOp (..))
import qualified Graphics.UI.Gtk             as Gtk


import           DBus
import           DBus.Client

{-
  panel <- withPanel 20 Top (OnMonitor 1) (xmonad --> clock <-- cpu)
  mpdPanel <- withPanel 20 Bottom SpanMonitors (mdpCurrentSong <> mpdVolume)
-}

data PanelMode = OnMonitor Int
               | SpanMonitors
               | OnScreen Int PanelMode


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
  { panelName     :: String
  , panelHeight   :: Int
  , panelPosition :: Position
  , panelMode     :: PanelMode
  , panelFgColor  :: Color
  , panelBgColor  :: Color }

defaultPanelConfig :: Theme -> PanelConfig
defaultPanelConfig theme = PanelConfig
  { panelName     = "defaultPanel"
  , panelHeight   = 20
  , panelPosition = Top
  , panelMode     = OnMonitor 0
  , panelFgColor  = fg $ normal theme
  , panelBgColor  = bg $ normal theme }

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

withPanel :: (PanelConfig -> PanelConfig) -> PanelItems -> Moon Panel
withPanel cf (PanelItems items) = do
  config <- (cf . defaultPanelConfig) <$> use theme
  let name = panelName config
  let mode = panelMode config

  withDisplay $ \display -> do
    (size, screen) <- liftIO $ getMode display mode
    window         <- liftIO Gtk.windowNew

    liftIO $ do
      Gtk.widgetSetName     window name
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
      bgColor $ panelBgColor config
      fgColor $ panelFgColor config

    (items, panel) <- configureWith (PanelState [] window box config) $
      sequence items

    forM_ items $ \(PanelItem n w p) ->
      liftIO $ Gtk.boxPackStart box w p 0

    on (sanatizeName name, name) withoutHelp $ \(action:_) ->
      case action of
        "show" -> do
          liftIO $ Gtk.widgetShow window
          pure $ "showing " ++ name
        "hide" -> do
          liftIO $ Gtk.widgetHide window
          pure $ "hiding " ++ name
        _      -> pure "Panel commands: show/hide"


    liftIO $ do
      Gtk.containerAdd window box
      Gtk.widgetShowAll window
      atomically $ newTVar $ panel { _panelItems = items }

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
