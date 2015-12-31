module Moonbase.Panel.Items.DBus
  ( dbusLabel
  , xmonadLog )
  where

import           Control.Lens

import qualified Graphics.UI.Gtk   as Gtk

import qualified DBus
import qualified DBus.Client       as DBus

import           Moonbase.DBus
import           Moonbase.Panel
import           Moonbase.Util
import           Moonbase.Util.Gtk


type DBusLabel = PanelItems IO

dbusLabel :: String -> DBus.MatchRule -> DBusLabel
dbusLabel name match = item $ do
        label   <- io $ Gtk.labelNew (Just "waiting for xmonad...")
        lift $ dbusSignal match $ \signal -> do
            let Just str = DBus.fromVariant $ head (DBus.signalBody signal) :: Maybe String
            io $ Gtk.postGUISync $ Gtk.labelSetMarkup label str

        return $ PanelItem name (Gtk.toWidget label) Gtk.PackNatural


xmonadLog :: DBusLabel
xmonadLog = dbusLabel "xmonadLog" rule
    where
        rule      = DBus.matchAny
          { DBus.matchPath      = Just path
          , DBus.matchInterface = Just interface
          , DBus.matchMember    = Just member }
        path      = DBus.objectPath_ "/org/moonbase/XMonadLog"
        interface = DBus.interfaceName_ "org.moonbase.XMonadLog"
        member    = DBus.memberName_ "Update"
