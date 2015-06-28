{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Moonbase.WM.XMonad where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad.State

import           Data.Maybe
import           Data.Monoid

import qualified Data.Map                     as M

import           Moonbase
import           Moonbase.Theme
import           Moonbase.WM.XMonad.Impl


import qualified Codec.Binary.UTF8.String     as Utf8
import qualified DBus                         as DBus
import qualified DBus.Client                  as DBus

import           Graphics.X11.Types

import           XMonad                       hiding (xmonad)
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout
import           XMonad.Layout.Circle
import           XMonad.Layout.GridVariants
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spacing
import           XMonad.Operations
import qualified XMonad.StackSet              as SS

import           XMonad.Actions.CopyWindow




withXMonad :: forall l. (LayoutClass l Window, Read (l Window)) =>
              ComponentM (Maybe ThreadId) (XConfig l)
            -> Moonbase ()

withXMonad genConfig  = withComponent High "xmonad" $
    newComponent Nothing $ do
        config <- genConfig
        thread <- liftIO $ forkIO $ moonbaseXMonad config
        put (Just thread)


simple_ gen = do
        rt <- moon $ get
        return $ moonX $ gen (Moonbase.terminal $ Moonbase.config $ rt) (dbus rt)

basic_ = default_ id

default_ f = do
    dbus  <- dbus <$> rt
    theme <- theme <$> rt
    term  <- Moonbase.terminal . Moonbase.config <$> rt

    return $ f $ moonX $ defaultConfig
        { modMask       = mod1Mask
        , borderWidth   = 2
        , logHook       = dynamicLogWithPP (prettyPrinter theme dbus)
        , layoutHook    = spacing 1 $ (Circle ||| Mirror tiled ||| tiled ||| noBorders Full)
        , XMonad.terminal      = term
        , mouseBindings = mouse
        , normalBorderColor = color_ $ disabledC theme
        , focusedBorderColor = color_ $ hlC1 theme
        , keys          = \x -> M.union (M.fromList (keys' term x)) (keys defaultConfig x)
        , workspaces    = ws }
  where
      rt      = moon $ get
      tiled   = Tall 1 (3/100) (1/2)
      bold' c = pangoColor' (color_ c) . wrap "<b>" "</b>" . pangoSanitize'
      prettyPrinter theme dbus = defaultPP
          { ppOutput  = dbusPPOutput dbus
          , ppTitle   = pangoSanitize'
          , ppCurrent = bold' (activeC theme)
          , ppVisible = bold' (normalC theme)
          , ppHidden  = bold' (normalC theme)
          , ppUrgent  = bold' (hlC2 theme)
          , ppLayout  = const ""
          , ppSep     = "  ~  "
          }
      ws = map show [1..9]

      mouse (XConfig {XMonad.modMask = modMask}) = M.fromList
        [ ((modMask, button1), \w -> focus w >> mouseMoveWindow w)
        , ((modMask, button2), \w -> focus w >> windows SS.swapMaster)
        , ((modMask, button3), \w -> focus w >> mouseResizeWindow w) ]

      keys' term conf@(XConfig {XMonad.modMask = modMask}) =
        [ ((modMask,                  xK_Return ), spawn term)
        , ((modMask .|. controlMask,  xK_x      ), kill1)
        , ((modMask,                  xK_t      ), withFocused $ windows . SS.sink)
        , ((modMask .|. shiftMask,    xK_equal  ), sendMessage $ IncMasterCols 1)
        , ((modMask .|. shiftMask,    xK_minus  ), sendMessage $ IncMasterCols (-1))
        , ((modMask .|. controlMask,  xK_equal  ), sendMessage $ IncMasterRows 1)
        , ((modMask .|. controlMask,  xK_minus  ), sendMessage $ IncMasterRows (-1))
        ] ++
          [((m .|. modMask, k), windows $ f i)
            | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
            , (f, m) <- [(SS.greedyView, 0), (SS.shift, shiftMask)]
            ]

-- moonXMonad :: forall l. (LayoutClass l Window, Read (l Window)) => XConfig l -> XConfig l
moonX conf = conf
  { manageHook          = manageMoonbase <+> manageHook conf <+> manageDocks
  , handleEventHook     = handleEventHook conf <+> ewmhDesktopsEventHook
  , startupHook         = startupHook conf <+> ewmhDesktopsStartup
  , layoutHook          = noBorderOn "MoonbasePrompt" $ avoidStruts $ layoutHook conf }
  where
    x <+> y = mappend x y
    tiled   = Tall 1 (3/100) (1/2)

    manageMoonbase = composeAll
      [ stringProperty "WM_WINDOW_ROLE" =? "MoonbasePrompt" --> doFloat
      , className =? "moonbase-test" --> doFloat ]

dbusPP :: DBus.Client -> PP
dbusPP dbus = defaultPP { ppOutput = dbusPPOutput dbus }

dbusPPOutput :: DBus.Client -> String -> IO ()
dbusPPOutput dbus str = DBus.emit dbus signal
    where
        signal    = (DBus.signal path interface member) { DBus.signalBody = [body] }
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


data NoBorderOnRole a = NoBorderOnRole String [a] deriving (Show, Read)

instance LayoutModifier NoBorderOnRole Window where
    unhook (NoBorderOnRole _ s) = asks (borderWidth . XMonad.config) >>= setBorders s

    redoLayout (NoBorderOnRole role s) _ _ wrs = do
        bw <- borderWidth . XMonad.config <$> ask

        setBorders ws 0
        withDisplay $ \d -> void $ forM ws $ \w -> do
            XMonad.io $ putStrLn $ "run on window = " ++ show w
            hasRole <- runQuery (stringProperty "WM_WINDOW_ROLE" =? role) w
            if hasRole
               then XMonad.io $ putStrLn "  >> has Role"
               else XMonad.io $ setWindowBorderWidth d w bw >> (putStrLn "  !! no role")
        return (wrs, Just (NoBorderOnRole role ws))
     where
       ws = map fst wrs


noBorderOn :: (LayoutClass l Window) => String -> l Window -> ModifiedLayout NoBorderOnRole l Window
noBorderOn role = ModifiedLayout $ NoBorderOnRole role []

setBorders :: [Window] -> Dimension -> X ()
setBorders ws bw = withDisplay $ \d -> mapM_ (\w -> XMonad.io $ setWindowBorderWidth d w bw) ws

