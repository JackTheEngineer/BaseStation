{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE BangPatterns #-}
  
module Main where

import GI.Cairo 
import Graphics.Rendering.Cairo as Cairo
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))
import Foreign.Ptr (castPtr)

import Control.Monad
import Control.Concurrent as CC
import Control.Concurrent.STM
import Control.Monad.Trans.Reader

import qualified Data.Text as T 
import Data.Maybe
import Data.Monoid ((<>))
import GI.Gtk.Objects.Builder
import Data.GI.Base
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk 
import GI.Gtk hiding (main)

import GI.GLib.Functions
import GI.Gtk.Objects.Entry
import GI.Gtk.Objects.EntryBuffer

import Data.Time as Time
import Data.Time.Clock

import System.IO

import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Control.Lens
import System.Hardware.Serialport
import qualified Data.ByteString as B 
import qualified Data.ByteString.Lazy as BL 
import Data.IORef

import UartInterface
import PID_Optimization


type Channel = IORef [(Float, Float)]
newChannel = newIORef [] :: IO (Channel)
modifyChannel = atomicModifyIORef'
readChannel = readIORef


-- | This function bridges gi-cairo with the hand-written cairo
-- package. It takes a `GI.Cairo.Context` (as it appears in gi-cairo),
-- and a `Render` action (as in the cairo lib), and renders the
-- `Render` action into the given context.
renderWithContext :: GI.Cairo.Context -> ReaderT Cairo IO a -> IO (a)
renderWithContext ct rendered = withManagedPtr ct $ \p -> runReaderT rendered (Cairo (castPtr p))

getObject b name cast = builderGetObject b name >>= unsafeCastTo cast . fromJust

mapTuple f (a, b) = (f a, f b)

quaternionWellFormed q = ((q0 q) > 0.0) && ((q0 q) <= 1.0)

rs :: UTCTime -> Uart -> [Channel] -> IO ()
rs startTime serport uartPlots = do
  quats <- getAndDecodeMessage decodeQuaternions serport -- message between newlines
  case quats of
    Just qs -> do
      now <- Time.getCurrentTime
      let difftime = realToFrac $ diffUTCTime now startTime
          angles = map (quatAsListToEuler) qs
          -- accs = zip (replicate (length angles) difftime) angles
          z = zip (replicate (length angles) difftime) (map (flip (!!) 2) angles)
          y = zip (replicate (length angles) difftime) (map (flip (!!) 1) angles)
          x = zip (replicate (length angles) difftime) (map (flip (!!) 0) angles)
          
      (flip modifyChannel ) (\b -> (concat [z, b], ())) (uartPlots !! 0)
      (flip modifyChannel ) (\b -> (concat [y, b], ())) (uartPlots !! 1)
      (flip modifyChannel ) (\b -> (concat [x, b], ())) (uartPlots !! 2)
          
    Nothing -> return ()
  CC.threadDelay 8000

activateRender :: (Int, Int) -> (Channel, DrawingArea) -> IO ()
activateRender (width, height) (channel, cairoArea) = do
  widgetSetSizeRequest cairoArea (fromIntegral width) (fromIntegral height)
  
  on cairoArea #draw $ \context -> do
    values <- readChannel channel
    let chartRendered = render (chart values) (fromIntegral width, fromIntegral height)
        asRender = runBackend (defaultEnv bitmapAlignmentFns) chartRendered
        !rendered = (runRender asRender)
        -- As far as i understand, "!rendered" 
    renderWithContext context rendered
    return True
  return ()

main :: IO ()
main = do
  Gtk.init Nothing
  builder <- builderNewFromFile "baseStation.glade"
  mainWindow <- getObject builder "mainWindow" Window
  toplevelBox <- getObject builder "toplevelBox" Box
  pEntry <- getObject builder "pEntry" Entry
  dEntry <- getObject builder "dEntry" Entry
  iEntry <- getObject builder "iEntry" Entry
  errLabel <- getObject builder "errLabel" Label
  sendBtn <- getObject builder "sendBtn" Button
  clearErrBtn <- getObject builder "clearErrBtn" Button

  let num = 3
  channels <- replicateM num newChannel
  cairoareas <- replicateM num (new DrawingArea [])
  let chansAndCairos = zip channels cairoareas
  mapM_ (#add toplevelBox) cairoareas

  
  let size = (600, 200)
  mapM_ (activateRender size) chansAndCairos
  
  startTime <- Time.getCurrentTime

  serport <- openUart "/dev/ttyACM0"
  
  let sendParams = do
        pParam <- (read . T.unpack) <$> getEntryText pEntry :: IO(Float)
        dParam <- (read . T.unpack) <$> getEntryText dEntry :: IO(Float)
        iParam <- (read . T.unpack) <$> getEntryText iEntry :: IO(Float)
        print pParam
        print dParam
        bangSerPort serport pParam dParam iParam
        return ()

  on mainWindow #destroy $ do
    closeUart serport
    mainQuit
    
  on mainWindow #keyPressEvent $ \event -> do
    name <- event `get` #keyval >>= Gdk.keyvalName
    when (name == Just "Escape") mainQuit
    return False

  on sendBtn #clicked $ sendParams
  _ <- forkIO $ Control.Monad.forever $ rs startTime serport channels
  
  timeoutAdd 0 200 $ (do
                        mapM_ widgetQueueDraw cairoareas
                        return True
                    )
  #showAll mainWindow
  Gtk.main

setLinesBlue :: PlotLines a b -> PlotLines a b
setLinesBlue = plot_lines_style . line_color .~ opaque blue

chart :: [(Float, Float)] -> Renderable ()
chart xyTuples = toRenderable layout
  where
    fontsize = 16
    sinusoid2 = plot_points_style .~ filledCircles 1.5 (opaque blue)
              $ plot_points_values .~ xyTuples
              $ plot_points_title .~ "Magic"
              $ def

    layout = layout_title .~ "MagicLines"
             $ layout_plots .~ [toPlot sinusoid2]
             $ layout_x_axis . laxis_title .~ "Time"
             $ layout_y_axis . laxis_title .~ "Amplitude"
             $ layout_all_font_styles . font_size .~ fontsize
             $ def  
