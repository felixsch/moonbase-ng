module Moonbase.Util.Widget.Chart where

import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.Rendering.Cairo as Cairo
import qualified System.Glib as Glib

import System.IO.Unsafe (unsafePerformIO)

import Control.Monad
import Control.Lens
import qualified Data.Vector as Vec
import qualified Data.Sequence as Seq
import qualified Data.Map as Map

import Moonbase.Core
import Moonbase.Theme
import Moonbase.Util.Gtk


-- DataTableValue --------------------------------------------------------------
data DataTableValue b = JustValue b

-- DataTableRow ----------------------------------------------------------------
data DataTableRow b = DataTableRow
  { tableColor :: Maybe Color
  , tableRow :: Seq.Seq (DataTableValue b) }

newRow :: Maybe Color -> [DataTableValue b] -> DataTableRow b
newRow color values = DataTableRow color $ Seq.fromList values

everyValueM_ :: (Monad m) => (Int -> DataTableValue b -> m ()) -> DataTableRow b -> m ()
everyValueM_ f (DataTableRow _ values)= Seq.foldlWithIndex (const f) (return ()) values

-- DataTable -------------------------------------------------------------------
data (Show a) => DataTable a b = DataTable (Map.Map a (DataTableRow b))

dataFromList :: (Ord a, Show a) => [(a, [b])] -> DataTable a b
dataFromList = DataTable . Map.fromList . genRow . genValues
  where
    genValues :: [(a, [b])] -> [(a, [DataTableValue b])]
    genValues x = traverse . _2 . traverse %~ JustValue $ x

    genRow :: [(a, [DataTableValue b])] -> [(a, DataTableRow b)]
    genRow s = traverse . _2 %~ newRow Nothing $ s

everyRowM_ :: (Show a, Monad m) => (a -> DataTableRow b -> m ()) -> DataTable a b -> m ()
everyRowM_ f (DataTable table)= Map.foldlWithKey (const f) (return ()) table

-- ChartConfig -----------------------------------------------------------------

data ChartConfig = ChartConfig
  { chartShowLabels :: Bool
  , chartDrawFrame :: Bool
  , chartMin :: Maybe Int
  , chartMax :: Maybe Int }

defaultChartConfig :: ChartConfig
defaultChartConfig = ChartConfig
  { chartShowLabels = True
  , chartDrawFrame  = False
  , chartMin        = Nothing
  , chartMax        = Nothing }

-- ChartRenderer ---------------------------------------------------------------
class ChartRenderer a where
  chartRenderWith :: a
                  -> (Int, Int)
                  -> ChartConfig
                  -> DataTable n v
                  -> Cairo.Render ()

-- Chart - Area ----------------------------------------------------------------
-- Chart - Line ----------------------------------------------------------------
-- Chart - Bar -----------------------------------------------------------------
-- Chart -----------------------------------------------------------------------

data Chart = Chart { chartArea :: Gtk.DrawingArea }

class Gtk.WidgetClass o => ChartClass o

toGraph :: Gtk.DrawingAreaClass o => o -> Chart
toGraph = Chart . Gtk.toDrawingArea

instance Glib.GObjectClass Chart where
  toGObject         = Glib.toGObject . chartArea
  unsafeCastGObject = Chart . Glib.unsafeCastGObject

instance Gtk.WidgetClass Chart


maybeChartConfig :: Gtk.Attr Chart (Maybe ChartConfig)
maybeChartConfig = unsafePerformIO Glib.objectCreateAttribute
{-# NOINLINE maybeChartConfig #-}

maybeChartData :: Gtk.Attr Chart (Maybe (DataTable a b))
maybeChartData = unsafePerformIO Glib.objectCreateAttribute
{-# NOINLINE maybeChartData #-}

chartConfig :: Gtk.Attr Chart ChartConfig
chartConfig = Gtk.newAttr get' set'
  where
    get' chart        = maybe fetchingFailed return =<< Gtk.get chart maybeChartConfig
    set' chart config = do
      Gtk.set chart [maybeChartConfig Gtk.:= Just config]
      ioasync $ Gtk.widgetQueueDraw chart
    fetchingFailed = error "Could not fetch chart configuration..."

chartData :: Gtk.Attr Chart (DataTable a b)
chartData = Gtk.newAttr get' set'
  where
    get' chart     = maybe fetchingFailed return =<< Gtk.get chart maybeChartData
    set' chart dat = do
      Gtk.set chart [maybeChartData Gtk.:= Just dat]
      ioasync $ Gtk.widgetQueueDraw chart
    fetchingFailed = error "Could not fetch chart data..."

chartNew :: (ChartRenderer renderer) => renderer -> IO Chart
chartNew renderer = do
  chart <- Chart <$> Gtk.drawingAreaNew
  Gtk.set chart [maybeChartConfig Gtk.:= Just defaultChartConfig]
  _ <- Gtk.on chart Gtk.draw $ do
    dat    <- liftIO $ Gtk.get chart chartData
    config <- liftIO $ Gtk.get chart chartConfig
    sizes  <- liftIO $ getSize chart
    void $ chartRenderWith renderer sizes config dat
  return chart
  where
    getSize chart = do
      area <- Gtk.widgetGetWindow chart

      case area of
           Nothing  -> return (0,0)
           Just win -> do
              w <- Gtk.drawWindowGetWidth win
              h <- Gtk.drawWindowGetHeight win
              return (w, h)

chartNewWithData :: (ChartRenderer renderer) => renderer -> DataTable a b -> IO Chart
chartNewWithData = undefined


-- Testing ---------------------------------------------------------------------

example :: DataTable String Integer
example = dataFromList [("cpu0", [10,23,89,12,90]), ("cpu1", [0,11,29,100,100])]
