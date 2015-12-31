module Moonbase.Panel.Items.Tray
  ( systemTray
  ) where

import           Control.Applicative
import           Control.Monad

import qualified Graphics.UI.Gtk                  as Gtk
import qualified Graphics.UI.Gtk.Misc.TrayManager as Gtk

import           Moonbase.DBus
import           Moonbase.Panel
import           Moonbase.Util
import           Moonbase.Util.Gtk

type SystemTray m = PanelItems m

systemTray :: (Moon m) => SystemTray m
systemTray = item $ do
  trayBox <- io $ do
    box         <- Gtk.hBoxNew False 5
    trayManager <- Gtk.trayManagerNew
    Just screen <- Gtk.screenGetDefault
    Gtk.trayManagerManageScreen trayManager screen
    Gtk.on trayManager Gtk.trayIconAdded $ \w -> do
      ioasync $ Gtk.widgetShowAll w
      Gtk.boxPackStart box w Gtk.PackNatural 0
    return box
  return $ PanelItem "systemTray" (Gtk.toWidget trayBox) Gtk.PackNatural
