module Moonbase.Util.Widget.Bar
  ( Bar(..)
  , BarOrientation(..)
  , BarConfig(..)
  , barNew
  , barConfig, barValue ) where

import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.Gtk          as Gtk
import qualified System.Glib              as Glib

import           System.IO.Unsafe         (unsafePerformIO)

import           Control.Lens
import           Control.Monad

import           Moonbase.Core
import           Moonbase.Theme
import           Moonbase.Util.Gtk


data BarOrientation = VerticalBar
                    | HorizontalBar
                    deriving (Show, Eq)

data BarConfig = BarConfig
  { barOrientation :: BarOrientation
  , barColor       :: Color
  , barMin         :: Int
  , barMax         :: Int
  , barWidth       :: Int
  , barLabel       :: Maybe String }

data Bar = Bar { barArea :: Gtk.DrawingArea }

instance Gtk.WidgetClass Bar
class Gtk.WidgetClass o => BarClass o

toBar :: Gtk.DrawingAreaClass o => o -> Bar
toBar = Bar . Gtk.toDrawingArea

instance Glib.GObjectClass Bar where
  toGObject         = Glib.toGObject . barArea
  unsafeCastGObject = Bar . Glib.unsafeCastGObject

maybeBarConfig :: Gtk.Attr Bar (Maybe BarConfig)
maybeBarConfig = unsafePerformIO Glib.objectCreateAttribute
{-# NOINLINE maybeBarConfig #-}

maybeBarValue :: Gtk.Attr Bar (Maybe Int)
maybeBarValue = unsafePerformIO Glib.objectCreateAttribute
{-# NOINLINE maybeBarValue #-}

barConfig :: Gtk.Attr Bar BarConfig
barConfig = Gtk.newAttr get' set'
  where
    get' bar = maybe fetchingFailed return =<< Gtk.get bar maybeBarConfig
    set' bar config = do
      Gtk.set bar [maybeBarConfig Gtk.:= Just config]
      ioasync $ Gtk.widgetQueueDraw bar
    fetchingFailed = error "Could not fetch bar configuration..."

barValue :: Gtk.Attr Bar Int
barValue = Gtk.newAttr get' set'
  where
    get' bar       = maybe fetchingFailed return =<< Gtk.get bar maybeBarValue
    set' bar value = do
      Gtk.set bar [maybeBarValue Gtk.:= Just value]
      ioasync $ Gtk.widgetQueueDraw bar
    fetchingFailed = error "Could not fetch bars value"

barNew :: BarConfig -> IO Bar
barNew initc = do
  bar <- Bar <$> Gtk.drawingAreaNew
  Gtk.set bar [ maybeBarConfig Gtk.:= Just initc
              , maybeBarValue Gtk.:= Just (barMin initc) ]

  -- initalize bar size
  (_, h)  <- widgetGetSize bar
  Gtk.widgetSetSizeRequest bar (barWidth initc) h

  _ <- Gtk.on bar Gtk.draw $ do
    value  <- liftIO $ Gtk.get bar barValue
    config <- liftIO $ Gtk.get bar barConfig
    sizes  <- widgetGetSize bar
    liftIO $ putStrLn $ " :: value = " ++ show value
    liftIO $ putStrLn $ " :: sizes = " ++ show sizes
    void $ if barOrientation config == HorizontalBar
            then drawHorizontalBar sizes config value
            else drawVerticalBar sizes config value
  return bar


drawHorizontalBar :: (Int, Int) -> BarConfig -> Int -> Cairo.Render ()
drawHorizontalBar (w,h) config value = Cairo.setSourceRGBA r g b a >> Cairo.setLineWidth 1 >> drawBox >> drawBar
  where
    drawBox = Cairo.rectangle 0 0 (fromIntegral w) (fromIntegral h) >> Cairo.stroke
    drawBar = return ()
    (r,g,b,a) = parseColorTuple $ barColor config

drawVerticalBar (w,h) config value = return ()
