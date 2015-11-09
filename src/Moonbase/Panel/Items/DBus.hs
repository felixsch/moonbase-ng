module Moonbase.Panel.Items.DBus
  ( dbusLabel
  , xmonadLog )
  where

import Control.Lens

import qualified Graphics.UI.Gtk as Gtk

import qualified DBus
import qualified DBus.Client as DBus

import Moonbase.Panel
import Moonbase.DBus
import Moonbase.Util
import Moonbase.Util.Gtk


dbusLabel :: String -> DBus.MatchRule -> PanelItems
dbusLabel name match = item $ do
        label   <- liftIO $ Gtk.labelNew (Just "waiting for xmonad...")
        lift $ dbusSignal match $ \signal -> do
            let Just str = DBus.fromVariant $ head (DBus.signalBody signal) :: Maybe String
            liftIO $ Gtk.postGUISync $ Gtk.labelSetMarkup label str

        return $ PanelItem name (Gtk.toWidget label) Gtk.PackNatural


xmonadLog :: PanelItems
xmonadLog = dbusLabel "xmonadLog" rule
    where
        rule      = DBus.matchAny 
          { DBus.matchPath      = Just path
          , DBus.matchInterface = Just interface
          , DBus.matchMember    = Just member }
        path      = DBus.objectPath_ "/org/moonbase/XMonadLog"
        interface = DBus.interfaceName_ "org.moonbase.XMonadLog"
        member    = DBus.memberName_ "Update"
