{-
Copyright (C) 2015 Felix Schnizlein <felix@schnizle.in>
This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Moonbase.Theme
   Copyright   : Copyright (C) 2015 Felix Schnizlein
   License     : GNU GPL2 or higher
   Maintainer  : Felix Schnizlein <felix@schnizle.in>
   Stability   : alpha
   Portability : not portable
Theme implementation for Moonbase
-}
module Moonbase.Theme
  ( Color
  , color_
  , FontAttr(..)
  , Font(..)
  , bold, italic
  , size, sans, monospace, droid, droidMono
  , Theme(..)
  , Style(..)
  , fg, font, bg
  , defaultTheme
  , defaultColor
  ) where


import           Data.Char

-- * Color & helpers

-- | A simple hex based representation of a color
-- following the syntax #rrggbbaa. Html uses it as well.
type Color = String


-- | checks if a color is valid. This function returns magenta if a invalid color was entered.
-- for Example:
-- >>>  color_ "#ffffff"
-- "#ffffff"
-- >>>  color_ "#fff"
-- "#ffffff"
-- >>>  color_ "#abcxef"
-- "#ff00ff"
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


-- * Fonts & helpers

-- | font settings
data FontAttr = Default -- ^ No attribute at all, just plain text
              | Bold    -- ^ Bold text
              | Thin    -- ^ More thin than normal
              | Thick   -- ^ Something between bold and normal
              | Italic  -- ^ Italic font
              | Underline -- ^ Underline the font
              deriving (Show, Eq)

-- | Font definition
-- Each font has to be a explicit name to match. Size can be selected and attributes added
-- You can generate your own Font definitions by using the constructor.
-- e.g
-- >>> Font "Droid Sans" 12 []
-- >>> Font "Droid Sans Mono" 12 [Thin, Italic]
data Font = Font
  { fontName  :: String -- ^ the name of the font
  , fontSize  :: Int -- ^ size of the font in px
  , fontAttrs :: [FontAttr] } -- ^ attributes how the font should be displayed
  deriving (Show)

-- | marks a font as bold
bold :: Font -> Font
bold f = f { fontAttrs = Bold : fontAttrs f }

-- | marks a font as italic
italic :: Font -> Font
italic f = f { fontAttrs = Italic : fontAttrs f }

-- | set the size of the font
size :: Int -> Font -> Font
size size' font = font { fontSize = size' }

-- * Predefined fonts

-- | fallback sans
sans :: Font
sans = Font "Sans" 12 []

-- | fallback monospace
monospace :: Font
monospace = Font "Monospace" 12 []

-- | droid sans
droid :: Font
droid = Font "Droid Sans" 12 []

droidMono :: Font
droidMono = Font "Droid Sans Mono" 12 []

-- * Theme
-- All the basic theming of moonbase works with a Theme. You define your own theme or use the default one.
-- @
-- blackBackground :: Color
-- blackBackground = "#000"
--
-- myTheme :: Theme
-- myTheme = Theme
--  { normal    = Style #fff" sans blackBackground
--  , highlight = Style "#f00" sans blackBackground
--  , active    = Style "#0f0" sans blackBackground
--  , disabled  = Style "#151515" sans blackBackground
--  , frame     = Style "#0f0" sans blackBackground }
--
-- main :: IO ()
-- main = moonbase $ do
--   withTheme myTheme
--   ...
-- @



-- | To make it more easy all different modes come with a triple of settings
-- A foreground, font and a background color.
data Style = Style Color Font Color
  deriving (Show)

-- | get the foreground
fg :: Style -> Color
fg (Style c _ _) = c

-- | get the font definition
font :: Style -> Font
font (Style _ f _) = f

-- | get the background color
bg :: Style -> Color
bg (Style _ _ c) = c

-- TODO: Add a map which value is used where

-- | Minimal theme used to style everything if not set otherwise
data Theme = Theme
  { normal    :: Style    -- ^ when everything is normal
  , highlight :: Style    -- ^ Need to highlight something or a message
  , active    :: Style    -- ^ If something is active (e.g the border of xmonad)
  , disabled  :: Style    -- ^ if something is disabled
  , frame     :: Style }  -- ^ the frame of something (e.g bar)
  deriving (Show)

-- | moonbase default theme
defaultTheme :: Theme
defaultTheme = Theme
  { normal    = Style "#ffffff" sans          defaultBg
  , highlight = Style "#268BD2" (bold sans)   defaultBg
  , active    = Style "#9ec400" sans          defaultBg
  , disabled  = Style "#808080" (italic sans) defaultBg
  , frame     = Style "#151515" sans          defaultBg }
    where
      defaultBg = "#242424"

-- | the default fallback color: magenta
defaultColor :: Color
defaultColor = "#ff00ff"
