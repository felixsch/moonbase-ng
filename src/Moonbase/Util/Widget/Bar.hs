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
import           Moonbase.Util.Cairo
import           Moonbase.Util.Gtk


data BarOrientation = VerticalBar
                    | HorizontalBar
                    deriving (Show, Eq)

data BarConfig = BarConfig
  { barOrientation  :: BarOrientation
  , barSegmentColor :: Color
  , barFrameColor   :: Color
  , barTextColor    :: Color
  , barMin          :: Int
  , barMax          :: Int
  , barWidth        :: Int }

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
    void $ if barOrientation config == HorizontalBar
            then drawHorizontalBar sizes config value
            else drawVerticalBar sizes config value
  return bar

drawHorizontalBar :: (Double, Double) -> BarConfig -> Int -> Cairo.Render ()
drawHorizontalBar (w,h) config value = drawSegments >> drawFrame
  where
    drawFrame = sourceColor (barFrameColor config)
              >> Cairo.setLineWidth 2
              >> Cairo.rectangle spaceW (gapH*4) (w - spaceW) (gapH*5)
              >> Cairo.stroke
    drawSegments = sourceColor (barSegmentColor config)
                 >> Cairo.rectangle (w - spaceW - segmentW) (gapH*4) (w - spaceW) (gapH*5)
                 >> Cairo.fill
    gapH     = h / 12
    line     = 2.0
    spaceW   = 2.0
    segmentW = (w - spaceW) * (fromIntegral value / 100.0)


drawVerticalBar :: (Double, Double) -> BarConfig -> Int -> Cairo.Render ()
drawVerticalBar (w,h) config value = drawSegments >> drawFrame
  where
    drawFrame = sourceColor (barFrameColor config)
              >> Cairo.setLineWidth 2
              >> Cairo.rectangle spaceW gapH (w- spaceW) gapH
              >> Cairo.stroke
    drawSegments = sourceColor (barSegmentColor config)
                 >> Cairo.rectangle spaceW gapH (w - spaceW) (segmentH - gapH)
                 >> Cairo.fill
    gapH     = 2.0
    spaceW   = 2.0
    segmentH = (h - gapH) * (fromIntegral value / 100.0)
