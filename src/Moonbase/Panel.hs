{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-
Copyright (C) 2015 Felix Schnizlein <felix@schnizle.in>
This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Moonbase.Panel
   Copyright   : Copyright (C) 2015 Felix Schnizlein
   License     : GNU GPL2 or higher
   Maintainer  : Felix Schnizlein <felix@schnizle.in>
   Stability   : alpha
   Portability : not portable
   A easy to use panel. To use the panel setup a panel configuration and and add some items

  simple example:
  >  import Moonbase.Panel
  >  import Moonbase.Panel.Items
  >
  >  myPanelConfig :: PanelConfig -> PanelConfig
  >  myPanelConfig conf = conf
  >    { panelName        = "myTopPanel"
  >    , panelOrientation = Top
  >    , panelMode        = OnMonitor 0  -- the first monitor
  >    }
  >
  >  main :: IO ()
  >  main = moonbase $ do
  >    panel <- withPanel myPanelConfig (xmonadLog --> notify <-- cpuBar defaultCpuBarConfig <> systemTray <> clock "%H:%M")
  >
  Easy right?
-}
module Moonbase.Panel
  ( PanelMode(..)
  , PanelConfig(..)
  , defaultPanelConfig
  , PanelItem(..)
  , panelItemName, panelItemWidget, panelItemPacking
  , PanelItems(..)
  , item
  , Panel(..)
  , withPanel
  ) where

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

-- | How and where to place the panel
-- If you want to use multiple screens use 'OnScreen' to specify which screen to use
-- e.g
--
-- > panelMode = OnScreen 1 (OnMonitor 0) -- second screen, first monitor
data PanelMode = OnMonitor Int          -- ^ place panel on a specific monitor
               | SpanMonitors           -- ^ try to span over multiple monitors
               | OnScreen Int PanelMode -- ^ if you use more than von virtual
                                        --   screen (multiple X Screens) you can specify which
                                        --   one to use.


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


-- | panel configuration use this to set your own settings.
-- 'withPanel' provides a pre-initialized panel configuration where
-- values are already set, depending on your theme configuration.
-- Caution: Panel name must be a valid DBus Name (do not any special character. Use CamelCase)
data PanelConfig = PanelConfig
  { panelName     :: String     -- ^ panel name to use in moonbase cli and DBus
  , panelHeight   :: Int        -- ^ panel height
  , panelPosition :: Position   -- ^ panel poisiton (Top, Bottom, Custom Int)
  , panelMode     :: PanelMode  -- ^ panel mode
  , panelFgColor  :: Color      -- ^ text color of the panels normal output
  , panelBgColor  :: Color }    -- ^ background color of the panel


-- | Moonbase default panel configuration.
defaultPanelConfig :: Theme -> PanelConfig
defaultPanelConfig theme = PanelConfig
  { panelName     = "defaultPanel"
  , panelHeight   = 20
  , panelPosition = Top
  , panelMode     = OnMonitor 0
  , panelFgColor  = fg $ normal theme
  , panelBgColor  = bg $ normal theme }

-- | item structure.
-- Lens are generated and exported
data PanelItem = PanelItem
  { _panelItemName    :: Name           -- ^ name of the panel without special characters and only camelcase
  , _panelItemWidget  :: Gtk.Widget     -- ^ widget of the item
  , _panelItemPacking :: Gtk.Packing }  -- ^ packing method (checkout box packing if you need more information)

makeLenses ''PanelItem

-- | PanelItems is the container type for every Panel Item
data PanelItems = PanelItems [Configure PanelState PanelItem]

instance Monoid PanelItems where
    mempty  = PanelItems []
    mappend (PanelItems a) (PanelItems b) = PanelItems (a ++ b)

-- | create a panel item
-- To implement your own panel item this function can be used.
-- example:
--
-- >  simpleLabel :: PanelItems
-- >  simpleLabel = item $ do
-- >   label <- liftIO $ Gtk.labelNew (Just "sample")
-- >   return $ PanelItem "nameOfTheItemInCamelCase" (Gtk.toWidget label) Gtk.PackNatural
--
item :: Configure PanelState PanelItem -> PanelItems
item gen = PanelItems [gen]


data PanelState = PanelState
  { _panelItems  :: [PanelItem]
  , _panelWindow :: Gtk.Window
  , _panelHBox   :: Gtk.HBox
  , _panelConfig :: PanelConfig }

makeLenses ''PanelState

-- | Panel Type
type Panel = TVar PanelState

-- | create a new panel. It preconfigures a panel configuration which can be changed
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
