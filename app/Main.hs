{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE BangPatterns #-}
  
module Main where

import Control.Monad.Trans.Reader (runReaderT)
import GI.Cairo 
import Graphics.Rendering.Cairo as Cairo
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))
import Foreign.Ptr (castPtr)

import Control.Monad
import Control.Concurrent as CC
import Control.Concurrent.STM

import Data.IORef

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
import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart.Backend.Cairo
import Control.Lens
import System.Hardware.Serialport
import qualified Data.ByteString as B 
import qualified Data.ByteString.Lazy as BL 
import qualified Data.ByteString.Builder as Q

import UartInterface
import PID_Optimization


type Channel = IORef [(Float, Float)]
newChannel = newIORef [] :: IO (Channel)
modifyChannel = modifyIORef'
readChannel = readIORef


-- | This function bridges gi-cairo with the hand-written cairo
-- package. It takes a `GI.Cairo.Context` (as it appears in gi-cairo),
-- and a `Render` action (as in the cairo lib), and renders the
-- `Render` action into the given context.
renderWithContext :: GI.Cairo.Context -> Render (a) -> IO (a)
renderWithContext ct r = let !rendered = (runRender r) in 
  withManagedPtr ct $ \p -> runReaderT rendered (Cairo (castPtr p))

getObject b name cast = builderGetObject b name >>= unsafeCastTo cast . fromJust

mapTuple f (a, b) = (f a, f b)

quaternionWellFormed q = ((q0 q) > 0.0) && ((q0 q) <= 1.0)

rs :: UTCTime -> IORef Q.Builder -> SerialPort -> Channel -> IO()
rs startTime buffer serport uartPlot = do
  freshIncomeBytes <- recv serport 4096
  atomicModifyIORef' buffer (\existingBytes -> (existingBytes <> (Q.byteString freshIncomeBytes), ()))
  contents <- readIORef buffer
  let concattedBS = Q.toLazyByteString contents
  case (parseDroneMessage concattedBS) of
    Right (bs_rest, _, result) -> do
      --      print $ mconcat ["RestLength: ",  show (B.length bs_rest)]
      atomicWriteIORef buffer (Q.byteString bs_rest)
      now <- Time.getCurrentTime
      let difftime = realToFrac $ diffUTCTime now startTime
          angles = map ((\x -> x!!2) . quatAsListToEuler) result
          accs = zip (replicate (length angles) difftime) angles
      atomicModifyIORef' uartPlot (\b -> (concat [accs, b]  , ()))
      
    Left (bs_rest, position, errstring) -> do
      print $ mconcat [errstring,
                       " at Position: ",
                       show position]
      let chopped = (((BL.drop 1) . (BL.dropWhile (/= 10))) concattedBS)
      atomicWriteIORef buffer (Q.lazyByteString chopped)


main :: IO ()
main = do
  Gtk.init Nothing
  builder <- builderNewFromFile "baseStation.glade"
  mainWindow <- getObject builder "mainWindow" Window
  canvas <- getObject builder "cairoArea" DrawingArea
  pEntry <- getObject builder "pEntry" Entry
  dEntry <- getObject builder "dEntry" Entry
  iEntry <- getObject builder "iEntry" Entry
  errLabel <- getObject builder "errLabel" Label
  sendBtn <- getObject builder "sendBtn" Button
  clearErrBtn <- getObject builder "clearErrBtn" Button

  

  let sz = (600, 400)
      szd = mapTuple fromIntegral sz

  startTime <- Time.getCurrentTime
  timekeeper <- newIORef startTime
  uartResultValues <- newIORef [] :: IO (IORef [(Float, Float)])

  on canvas #draw $ \context -> do
    values <- readIORef uartResultValues
    let rendered = runBackend (defaultEnv bitmapAlignmentFns) (render (chart values) szd)
    renderWithContext context rendered
    return True

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

  buffer <- newIORef (Q.byteString B.empty) :: IO (IORef Q.Builder)
  
  timeoutAdd 0 8 $ ( do
                        rs startTime buffer serport uartResultValues
                        widgetQueueDraw canvas
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
