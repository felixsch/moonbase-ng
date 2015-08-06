
import Moonbase
import Moonbase.Theme
import Moonbase.WM.XMonad


term :: Terminal
term (Just args') = exec $ appWith "gnome-terminal" [args']
term Nothing     = exec $ app "gnome-terminal"

main :: IO ()
main = moonbase term $ do
  xmonad <- withDefaultXMonad defaultTheme
  exec $ app "gnome-terminal"
  exec $ app "d-feet"
  
  return ()
