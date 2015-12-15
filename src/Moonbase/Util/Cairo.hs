module Moonbase.Util.Cairo where

import qualified Graphics.Rendering.Cairo as Cairo

import           Moonbase.Core
import           Moonbase.Theme
import           Moonbase.Util.Gtk

sourceColor :: Color -> Cairo.Render ()
sourceColor color = Cairo.setSourceRGBA r g b a
  where
    (r,g,b,a) = clamp $ parseColorTuple color
