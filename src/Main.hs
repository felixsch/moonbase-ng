
import           Moonbase
import           Moonbase.Panel
import           Moonbase.Panel.Items
import           Moonbase.Panel.Items.Cpu
import           Moonbase.Theme
import           Moonbase.WM.XMonad

import           XMonad                     hiding ((-->))

import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.Circle
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace


term :: Terminal
term (Just args') = exec $ appWith "gnome-terminal" [args']
term Nothing     = exec $ app "gnome-terminal"

panel :: PanelConfig -> PanelConfig
panel conf = conf
  { panelName     = "top-panel"
  , panelHeight   = 20
  , panelPosition = Top
  , panelMode     = OnMonitor 0 }

-- panel items -------------------------------------------------------------------------------------
myCpuBar :: CpuBar
myCpuBar = cpuBarWithLabel CpuAll 800 defaultCpuBarConfig


myLayout = onWorkspace "9" (noBorders Full) $
  avoidStruts (Circle ||| Mirror tiled ||| tiled)
    where
      tiled = Tall 1 (3/100) (1/2)

myXMonadConfig config = return $ config
  { layoutHook = myLayout }


main :: IO ()
main = moonbase term $ do
  xmonad  <- withXMonad myXMonadConfig
  desktop <- withDesktop $
    onEveryMonitor $ setWallpaper "/tmp/bg1.jpg"
  p   <- withPanel panel (xmonadLog --> myCpuBar <> clock "%m.%d.%Y")
  exec $ app "gnome-terminal"
  exec $ app "d-feet"

  return ()
