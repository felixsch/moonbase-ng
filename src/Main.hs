
import           Moonbase
import           Moonbase.Panel
import           Moonbase.Panel.Items
import           Moonbase.Panel.Items.Cpu
import           Moonbase.Theme
import           Moonbase.WM.XMonad


term :: Terminal
term (Just args') = exec $ appWith "gnome-terminal" [args']
term Nothing     = exec $ app "gnome-terminal"

panel :: PanelConfig
panel = PanelConfig
  { panelName     = "top-panel"
  , panelHeight   = 20
  , panelPosition = Top
  , panelMode     = OnMonitor 0
  , panelStyle    = defaultTheme }

-- panel items -------------------------------------------------------------------------------------
myCpuBar :: CpuBar
myCpuBar = cpuBarWithLabel CpuAll 800 defaultCpuBarConfig


main :: IO ()
main = moonbase term $ do
  xmonad  <- withDefaultXMonad defaultTheme
  desktop <- withDesktop $
    onEveryMonitor $ setWallpaper "/tmp/bg1.jpg"
  p   <- withPanel panel (xmonadLog --> myCpuBar <> clock "%m.%d.%Y")
  exec $ app "gnome-terminal"
  exec $ app "d-feet"

  return ()
