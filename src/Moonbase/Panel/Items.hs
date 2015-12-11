module Moonbase.Panel.Items
  ( expandR, expandL, (<--), (-->), (<>)
  , dbusLabel, xmonadLog, systemTray, clock) where

import Data.Monoid

import Moonbase.Panel.Items.Expand
import Moonbase.Panel.Items.DBus
import Moonbase.Panel.Items.Tray
import Moonbase.Panel.Items.Clock
