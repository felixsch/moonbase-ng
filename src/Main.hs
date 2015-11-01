
import Moonbase
import Moonbase.Panel
import Moonbase.Theme
import Moonbase.WM.XMonad


term :: Terminal
term (Just args') = exec $ appWith "gnome-terminal" [args']
term Nothing     = exec $ app "gnome-terminal"

panelConfig :: PanelConfig
panelConfig = PanelConfig 
  { panelHeight   = 20
  , panelPosition = Top
  , panelMode     = OnMonitor 0 }

main :: IO ()
main = moonbase term $ do
  xmonad  <- withDefaultXMonad defaultTheme
  desktop <- withDesktop $
    onEveryMonitor $ setWallpaper "/tmp/bg1.jpg"
  panel   <- withPanel panelConfig $ PanelItems []
  exec $ app "gnome-terminal"
  exec $ app "d-feet"
  
  return ()
