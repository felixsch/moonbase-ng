{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Moonbase.WM.XMonad
  ( XMonad(..)
  , MoonbaseXMonadLayout
  , withXMonad
  , defaultXMonadConfig
  , defaultEWMHConfig
  , defaultKeyBindings
  , defaultMouseBindings
  , callMoonbase
  ) where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad.State

import qualified Data.Map                     as M
import           Data.Monoid

import           Moonbase
import           Moonbase.Core
import           Moonbase.Theme
import           Moonbase.WM.XMonad.Impl


import qualified Codec.Binary.UTF8.String     as Utf8
import qualified DBus
import qualified DBus.Client                  as DBus

import           Graphics.X11.Types

import           XMonad                       hiding (xmonad)
import           XMonad.Actions.CopyWindow
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.GridVariants
import           XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet              as SS
import           XMonad.Util.EZConfig



data XMonad = XMonad
  { workspaces :: [String]
  , threadId   :: ThreadId }

type MoonbaseXMonadLayout = ModifiedLayout AvoidStruts (Choose Tall (Choose (Mirror Tall) Full))

withXMonad :: (Read (l Window), LayoutClass l Window) => (XConfig MoonbaseXMonadLayout -> Moon (XConfig l)) -> Moon XMonad
withXMonad generator  = do
        theme  <- use theme
        client <- use dbus
        generated <- generator $ defaultXMonadConfig client theme
        thread <- liftIO $ forkIO $ moonbaseXMonad generated
        liftIO $ threadDelay 2000000
        return $ XMonad (XMonad.workspaces generated) thread

dbusTerminalCall :: String
dbusTerminalCall = "dbus-send ..."


defaultXMonadConfig :: DBusClient -> Theme -> XConfig MoonbaseXMonadLayout
defaultXMonadConfig client theme = defaultEWMHConfig client theme $ XMonad.defaultConfig
    { XMonad.terminal    = dbusTerminalCall
    , XMonad.workspaces  = map show [1..5]
    , borderWidth        = 2
    , mouseBindings      = defaultMouseBindings
    , keys               = keyBinding
    , normalBorderColor  = bg $ normal theme
    , focusedBorderColor = fg $ active theme }
    where
      keyBinding conf = M.union
        (defaultKeyBindings conf)
        (keys defaultConfig conf)

defaultEWMHConfig :: DBusClient -> Theme -> XConfig (Choose Tall (Choose (Mirror Tall) Full)) -> XConfig MoonbaseXMonadLayout
defaultEWMHConfig dbus theme conf = conf
  { manageHook      = manageHook conf <> manageDocks
  , handleEventHook = ewmhDesktopsEventHook <> handleEventHook conf
  , startupHook     = ewmhDesktopsStartup <> startupHook conf
  , layoutHook      = avoidStruts $ layoutHook conf
  , logHook         = ewmhDesktopsLogHook <> dbusPanelLog theme dbus <> logHook conf }


defaultKeyBindings :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
defaultKeyBindings conf@(XConfig {XMonad.modMask = mask}) = mkKeymap conf
  [ ("M-<Return>", void $ callMoonbase "spawn" ["terminal"])
  , ("M-C-x", kill1)
  , ("M-t", withFocused $ windows . SS.sink)
  , ("M-v", sendMessage $ IncMasterCols 1)
  , ("M-s", sendMessage $ IncMasterRows 1)
  , ("M-S-v", sendMessage $ IncMasterCols (-1))
  , ("M-S-s", sendMessage $ IncMasterRows (-1))]
  `M.union`
    workspaceKeys
  where
    workspaceKeys = M.fromList
      [ ((m .|. mask, k), windows $ f i)
       | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
       , (f, m) <- [(SS.greedyView, 0), (SS.shift, shiftMask)]
      ]


defaultMouseBindings :: XConfig l -> M.Map (ButtonMask, Button) (Window -> X ())
defaultMouseBindings (XConfig {XMonad.modMask = mask}) = M.fromList
  [ ((mask, button1), \w -> focus w >> mouseMoveWindow w)
  , ((mask, button2), \w -> focus w >> windows SS.swapMaster)
  , ((mask, button3), \w -> focus w >> mouseResizeWindow w) ]


callMoonbase :: Name -> [String] -> X (Maybe String)
callMoonbase action' args' = liftIO $ runMoonbaseAction action' args'

dbusPanelLog :: Theme -> DBusClient -> X ()
dbusPanelLog theme client = dynamicLogWithPP pretty
  where
    pretty = defaultPP
      { ppOutput  = dbusPPOutput client
      , ppTitle   = pangoSanitize'
      , ppCurrent = withColor (active theme)
      , ppVisible = withColor (active theme)
      , ppHidden  = withColor (active theme)
      , ppUrgent  = withColor (active theme)
      , ppLayout  = const ""
      , ppSep     = "  ~  "
      }

dbusPPOutput :: DBusClient -> String -> IO ()
dbusPPOutput client str = DBus.emit client signal'
    where
        signal'   = (DBus.signal path interface member) { DBus.signalBody = [body] }
        path      = DBus.objectPath_ "/org/moonbase/XMonadLog"
        interface = DBus.interfaceName_ "org.moonbase.XMonadLog"
        member    = DBus.memberName_ "Update"
        body      = DBus.toVariant $ Utf8.decodeString str

pangoColor' :: String -> String -> String
pangoColor' fg str = left ++ str ++ right
    where
        left = "<span foreground=\"" ++ fg ++ "\">"
        right = "</span>"

pangoSanitize' :: String -> String
pangoSanitize' = foldr sanitize ""
  where
    sanitize '>' xs = "&gt;" ++ xs
    sanitize '<' xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&' xs = "&amp;" ++ xs
    sanitize x xs = x:xs


withColor :: Style -> String -> String
withColor (Style c _ _) = pangoColor' c
