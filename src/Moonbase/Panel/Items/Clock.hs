module Moonbase.Panel.Items.Clock
  ( clock, clockWith
  ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad

import qualified Graphics.UI.Gtk     as Gtk

import           Data.Time.Format
import           Data.Time.LocalTime

import           Moonbase.DBus
import           Moonbase.Panel
import           Moonbase.Theme
import           Moonbase.Util
import           Moonbase.Util.Gtk

clock :: (Moon m) => String -> PanelItems m
clock fmt = item $ do
        label <- io $ createClockWidget fmt 1 Nothing
        return $ PanelItem "date" (Gtk.toWidget label) Gtk.PackNatural

clockWith :: (Moon m) => String -> Int -> Color -> PanelItems m
clockWith fmt poll color = item $ do
        label <- io $ createClockWidget fmt poll (Just color)
        return $ PanelItem "dateWith" (Gtk.toWidget label) Gtk.PackNatural

createClockWidget :: String -> Int -> Maybe Color -> IO Gtk.Label
createClockWidget fmt poll color = do
        l <- Gtk.labelNew (Just "-")
        Gtk.labelSetUseMarkup l True
        _ <- Gtk.on l Gtk.realize $ void $
            forkIO $ forever $ do
                str <- formatTime defaultTimeLocale fmt <$> getZonedTime
                Gtk.postGUISync $ Gtk.labelSetMarkup l $ format str
                threadDelay (1000000 * poll)
        return l
  where
      format str = case color of
                        Just x  -> pangoColor x str
                        Nothing -> str
