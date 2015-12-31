module Moonbase.Panel.Items.Expand
  ( expandL, expandR
  , (<--), (-->) ) where

import qualified Graphics.UI.Gtk as Gtk

import           Moonbase.Panel
import           Moonbase.Util

type Expander m = PanelItems m


expandR :: (Moon m) => Maybe String -> Expander m
expandR mlabel = item $ do
  label <- io $ Gtk.labelNew mlabel
  return $ PanelItem "spacer-right" (Gtk.toWidget label) Gtk.PackGrow


(-->) :: (Moon m) => PanelItems m -> PanelItems m -> PanelItems m
(PanelItems a) --> (PanelItems b) = PanelItems $ a ++ spacer ++ b
  where
    (PanelItems spacer) = expandR Nothing


expandL :: (Moon m) => Maybe String -> Expander m
expandL mlabel = item $ do
  label <- io $ Gtk.labelNew mlabel
  return $ PanelItem "spacer-left" (Gtk.toWidget label) Gtk.PackGrow


(<--) :: (Moon m) => PanelItems m -> PanelItems m -> PanelItems m
(PanelItems a) <-- (PanelItems b) = PanelItems $ a ++ spacer ++ b
  where
    (PanelItems spacer) = expandL
