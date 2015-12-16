module Moonbase.Theme
  ( Color
  , color_
  , FontAttr(..)
  , Font(..)
  , bold, italic
  , size, sans, monospace
  , Theme(..)
  , Style(..)
  , fg, font, bg
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

fg :: Style -> Color
fg (Style c _ _) = c

font :: Style -> Font
font (Style _ f _) = f

bg :: Style -> Color
bg (Style _ _ c) = c

data Theme = Theme
  { normal    :: Style
  , highlight :: Style
  , active    :: Style
  , disabled  :: Style
  , frame     :: Style }
  deriving (Show)

defaultTheme :: Theme
defaultTheme = Theme
  { normal    = Style "#ffffff" sans          defaultBg
  , highlight = Style "#268BD2" (bold sans)   defaultBg
  , active    = Style "#9ec400" sans          defaultBg
  , disabled  = Style "#808080" (italic sans) defaultBg
  , frame     = Style "#151515" sans          defaultBg }
    where
      defaultBg = "#242424"

defaultColor :: Color
defaultColor = "#ff00ff"
