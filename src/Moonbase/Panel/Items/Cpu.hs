module Moonbase.Panel.Items.Cpu where
import           Control.Applicative
import           Control.Arrow
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class

import           System.IO

import qualified Graphics.UI.Gtk          as Gtk

import           Data.Time.Format
import           Data.Time.LocalTime

import           Moonbase.DBus
import           Moonbase.Panel
import           Moonbase.Theme
import           Moonbase.Util
import           Moonbase.Util.Css
import           Moonbase.Util.Gtk
import           Moonbase.Util.Widget.Bar


defaultCpuBarConfig :: BarConfig
defaultCpuBarConfig = BarConfig
  { barOrientation = HorizontalBar
  , barSegmentColor       = "#9ec400"
  , barFrameColor         = "#151515"
  , barTextColor         = "#151515"
  , barMin         = 0
  , barMax         = 100
  , barWidth       = 40 }

data Cpu = CpuAll
         | CpuCore Int

instance Show Cpu where
  show (CpuAll)    = "cpuAll"
  show (CpuCore i) = "core" ++ show i

data CpuStat = CpuStat
  { cpuUser    :: Int
  , cpuNice    :: Int
  , cpuSystem  :: Int
  , cpuIdle    :: Int
  , cpuIOWait  :: Int
  , cpuIRQ     :: Int
  , cpuSoftIRQ :: Int
  , cpuSteal   :: Int } deriving (Show)

type CpuBar = PanelItems

cpuBar :: Cpu -> Int -> BarConfig -> CpuBar
cpuBar cpu ms conf = item $ do
  bar <- liftIO $ barNew conf
  i   <- liftIO $ readStat cpu
  runCpuBar cpu ms i bar
  return $ PanelItem (show cpu ++ "Bar") (Gtk.toWidget bar) Gtk.PackNatural

cpuBarWithLabel :: Cpu -> Int -> BarConfig -> CpuBar
cpuBarWithLabel cpu ms conf = item $ do
  box <- liftIO $ do
    box   <- Gtk.hBoxNew False 2
    bar   <- barNew conf
    label <- Gtk.labelNew (Just "0%")
    i     <- readStat cpu
    (_, h) <- widgetGetSize label

    Gtk.labelSetJustify label Gtk.JustifyLeft
    Gtk.widgetSetName label $ show cpu ++ "-label"
    Gtk.widgetSetSizeRequest label 40 h
    Gtk.boxPackStart box bar Gtk.PackNatural 0
    Gtk.boxPackStart box label Gtk.PackNatural 0

    withCss label $ fgColor (barTextColor conf)

    pollForever 0 i $ \p -> do
      a <- readStat cpu
      threadDelay (ms * 1000)
      b <- readStat cpu
      let value = calcCpuLoad p a b
      Gtk.labelSetText label (show value ++ "%")
      Gtk.set bar [barValue Gtk.:= value]
      return b
    return box
  return $ PanelItem (show cpu ++ "Bar") (Gtk.toWidget box) Gtk.PackNatural


runCpuBar :: (MonadIO m) => Cpu -> Int -> CpuStat -> Bar -> m ()
runCpuBar cpu ms i bar = liftIO $ pollForever 0 i $ \p -> do
    a <- readStat cpu
    threadDelay (ms * 1000)
    b <- readStat cpu
    Gtk.set bar [barValue Gtk.:= calcCpuLoad p a b]
    return b

calcCpuLoad :: CpuStat -> CpuStat -> CpuStat -> Int
calcCpuLoad p a b = ((totalAll p a b - idleAll p a b) * 100) `div` totalAll p a b
  where
    idle c = cpuIdle c + cpuIOWait c
    nonIdle c = cpuUser c + cpuNice c + cpuSystem c + cpuIRQ c + cpuSoftIRQ c + cpuSteal c
    total c = idle c + nonIdle c
    totalAll p a b = total b - (total a + total p) `div` 2
    idleAll p a b  = idle b - (idle a + idle p) `div` 2


readStat :: Cpu -> IO CpuStat
readStat cpu= do
  hdl  <- openFile "/proc/stat" ReadMode
  stats <- readCpuStats hdl []
  hClose hdl
  return $ case cpu of
    CpuAll -> head stats
    CpuCore x  -> stats !! x
  where
    readCpuStats hdl stats = do
      (name, stat) <- parse <$> hGetLine hdl
      if name == "intr"
        then return stats
        else readCpuStats hdl (stats ++ [stat])

    parse = (head . words) &&& (mkCpuStat . map read . tail . words)

    mkCpuStat (u:n:s:i:io:irq:sirq:st:_) = CpuStat u n s i io irq sirq st
    mkCpuStat _                          = CpuStat 0 0 0 0 0  0   0    0
