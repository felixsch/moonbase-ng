module Moonbase.Theme
  ( Color
  , color_
  , FontAttr(..)
  , Font(..)
  , bold, italic
  , size, sans, monospace
  , Theme(..)
  , Style(..)
  , color, font, bg
  , DefaultTheme(..)
  , defaultTheme
  , defaultColor
  ) where


import           Data.Char


type Color = String


color_ :: Color -> Color
color_ ['#', r, g, b]                = ['#', r, r, g, g, b, b]
color_ ['#', r1, r2, g1, g2, b1, b2] = case checkHex [r1, r2, g1, g2, b1, b2] of
                                             Just x  -> '#' : x
                                             Nothing -> "#ff00ff"
color_ _                             = "#ff00ff"


checkHex :: String -> Maybe String
checkHex [] = Just []
checkHex (x:xs) = if isHexDigit x
                     then (x :) <$> checkHex xs
                     else Nothing


data FontAttr = Default
              | Bold
              | Thin
              | Thick
              | Italic
              | Underline
              deriving (Show, Eq)


data Font = Font
  { fontName  :: String
  , fontSize  :: Int
  , fontAttrs :: [FontAttr] }
  deriving (Show)


bold :: Font -> Font
bold f = f { fontAttrs = Bold : fontAttrs f } 


italic :: Font -> Font
italic f = f { fontAttrs = Italic : fontAttrs f }


size :: Int -> Font -> Font
size size' font = font { fontSize = size' }


sans :: Font
sans = Font "Sans" 12 []


monospace :: Font
monospace = Font "Monospace" 12 []



data Style = Style Color Font Color
  deriving (Show)

color :: Style -> Color
color (Style c _ _) = c

font :: Style -> Font
font (Style _ f _) = f

bg :: Style -> Color
bg (Style _ _ c) = c


class Theme a where 
  getNormal    :: a -> Style
  getHighlight :: a -> Style
  getActive    :: a -> Style
  getDisabled  :: a -> Style
  
data DefaultTheme = DefaultTheme
  { normal     :: Style
  , highlight  :: Style
  , active     :: Style
  , disabled   :: Style }
  deriving (Show)

instance Theme DefaultTheme where
  getNormal    = normal
  getHighlight = highlight
  getActive    = active
  getDisabled  = disabled


defaultTheme :: DefaultTheme
defaultTheme = DefaultTheme
  { normal    = Style "#ffffff" sans          defaultBg
  , highlight = Style "#268BD2" (bold sans)   defaultBg
  , active    = Style "#9ec400" sans          defaultBg
  , disabled  = Style "#808080" (italic sans) defaultBg }
    where
      defaultBg = "#242424"

defaultColor :: Color
defaultColor = "#ff00ff"







