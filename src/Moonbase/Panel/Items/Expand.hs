module Moonbase.Panel.Items.Expand
  ( expandL, expandR
  , (<--), (-->) ) where

import qualified Graphics.UI.Gtk as Gtk

import Moonbase.Panel
import Moonbase.Util

expandR :: Maybe String -> PanelItems
expandR mlabel = item $ do
  label <- liftIO $ Gtk.labelNew mlabel
  return $ PanelItem "spacer-right" (Gtk.toWidget label) Gtk.PackGrow

(-->) :: PanelItems -> PanelItems -> PanelItems
(PanelItems a) --> (PanelItems b) = PanelItems $ a ++ spacer ++ b
  where
    (PanelItems spacer) = expandR Nothing


expandL :: Maybe String -> PanelItems
expandL mlabel = item $ do
  label <- liftIO $ Gtk.labelNew mlabel
  return $ PanelItem "spacer-left" (Gtk.toWidget label) Gtk.PackGrow

(<--) :: PanelItems -> PanelItems -> PanelItems 
(PanelItems a) <-- (PanelItems b) = PanelItems $ a ++ spacer ++ b
  where
    (PanelItems spacer) = expandL Nothing

