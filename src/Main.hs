
import Moonbase
import Moonbase.Panel
import Moonbase.Theme
import Moonbase.WM.XMonad


term :: Terminal
term (Just args') = exec $ appWith "gnome-terminal" [args']
term Nothing     = exec $ app "gnome-terminal"

panel :: PanelConfig
panel = PanelConfig 
  { panelHeight   = 20
  , panelPosition = Top
  , panelMode     = OnMonitor 0
  , panelStyle    = defaultTheme }

main :: IO ()
main = moonbase term $ do
  xmonad  <- withDefaultXMonad defaultTheme
  desktop <- withDesktop $
    onEveryMonitor $ setWallpaper "/tmp/bg1.jpg"
  p   <- withPanel panel (xmonadLog)
  exec $ app "gnome-terminal"
  exec $ app "d-feet"
  
  return ()
