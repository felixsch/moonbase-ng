module Moonbase.Theme
  ( Color
  , color_
  , Attr(..)
  , HasAttr(..)
  , bold, italic, color
  , Font(..)
  , size, sans, monospace
  , Theme(..)
  , defaultTheme
  ) where

import           Control.Applicative

import           Data.Char
import           Data.List


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


data Attr = Default
          | Bold
          | Thin
          | Thick
          | Italic
          | Underline
          | Colorize Color
          | CustomAttr String
          deriving (Show, Eq)


class HasAttr a where
  attr :: Attr -> a -> a
  def  :: a


data Font = Font
  { fontName :: String
  , fontSize :: Int
  , fontAttr :: [Attr] }

instance HasAttr Font where
  attr attr' (Font fname fsize fattrs) = Font fname fsize $ nub (attr' : fattrs)
  def = Font "Sans" 12 []


bold :: HasAttr a => a -> a
bold = attr Bold


italic :: HasAttr a => a -> a
italic = attr Italic


color :: HasAttr a => Color -> a -> a
color color' = attr (Colorize (color_ color'))


size :: Int -> Font -> Font
size size' font = font { fontSize = size' }


sans :: Font
sans = Font "Sans" 12 []

monospace :: Font
monospace = Font "Monospace" 12 []

data Theme = Theme
  { normal     :: Font
  , bgNormal   :: Color
  , hl         :: Font
  , bgHl       :: Color
  , active     :: Font
  , bgActive   :: Color
  , disabled   :: Font
  , bgDisabled :: Color }


defaultTheme :: Theme
defaultTheme = Theme
  { normal     = color "#ffffff" sans
  , bgNormal   = defaultBg
  , hl         = color "#268BD2" $ bold sans
  , bgHl       = defaultBg
  , active     = color "#9ec400" sans
  , bgActive   = defaultBg
  , disabled   = color "#808080" $ italic sans
  , bgDisabled = defaultBg }
    where
      defaultBg = "#242424"









