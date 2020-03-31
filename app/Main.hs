{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE BangPatterns #-}
  
module Main where

import GI.Cairo 
import Graphics.Rendering.Cairo as Cairo
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))
import qualified Graphics.Rendering.Cairo as C
--import qualified Graphics.Rendering.Pango as P
import qualified GI.Pango as P
import Graphics.Rendering.Plot as Plot
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
import Control.Lens
import System.Hardware.Serialport

import Data.IORef

import UartInterface
import PID_Optimization

import qualified RIO.Vector.Storable as VS
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL

type Channel = IORef ([Double], [Double])
newChannel = newIORef ([], []) :: IO (Channel)
modifyChannel = atomicModifyIORef'
readChannel = readIORef


-- | This function bridges gi-cairo with the hand-written cairo
-- package. It takes a `GI.Cairo.Context` (as it appears in gi-cairo),
-- and a `Render` action (as in the cairo lib), and renders the
-- `Render` action into the given context.
renderWithContext :: GI.Cairo.Context -> Render () -> IO ()
renderWithContext ct r = withManagedPtr ct $ \p ->
                         runReaderT (runRender r) (Cairo (castPtr p))

getObject b name cast = builderGetObject b name >>= unsafeCastTo cast . fromJust

mapTuple f (a, b) = (f a, f b)

quaternionWellFormed q = ((q0 q) > 0.0) && ((q0 q) <= 1.0)

f2d :: Float -> Double
f2d = realToFrac

rs :: UTCTime -> Uart -> [Channel] -> IO ()
rs startTime serport plotDataChannels = do
  quats <- getAndDecodeMessage decodeQuaternions serport
  case quats of
    Just qs -> do
      now <- Time.getCurrentTime
      let difftime = realToFrac $ diffUTCTime now startTime
          angles = map (quatAsListToEuler) qs
          l = (length angles)
          zData = map (f2d . (flip (!!) 2)) angles
          yData = map (f2d . (flip (!!) 1)) angles
          xData = map (f2d . (flip (!!) 0)) angles
          newtimes = replicate l (f2d difftime)
          -- updater
          u = (\f d -> (( newtimes ++ (fst d), f ++ (snd d)), ())) 

      modifyChannel (plotDataChannels !! 0) (u zData)
      modifyChannel (plotDataChannels !! 1) (u yData)
      modifyChannel (plotDataChannels !! 2) (u xData)

    Nothing -> return ()
  CC.threadDelay 8000

activateRender :: (Int, Int) -> (Channel, DrawingArea) -> IO ()
activateRender (width, height) (channel, cairoArea) = do
  widgetSetSizeRequest cairoArea (fromIntegral width) (fromIntegral height)
  
  on cairoArea #draw $ \context -> do
    values <- readChannel channel
    let !rendered = Plot.render ((uncurry figure) values) (width, height) 
    renderWithContext context rendered
    return True
  return ()

main :: IO ()
main = do
  Gtk.init Nothing
  builder <- builderNewFromFile "baseStation.glade"
  mainWindow <- getObject builder "mainWindow" Window
  toplevelBox <- getObject builder "toplevelBox" GI.Gtk.Box
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

aat = VS.generate 1000 (\x -> 2*pi/1000 * (fromIntegral x)) :: VS.Vector Double
ax = sin aat

figure :: [Double] -> [Double] -> Figure()
figure x_s y_s = do
        withLineDefaults $ Plot.setLineWidth 0.2
        withTextDefaults $ setFontFamily "Cantarell"
        withTitle $ setText "Drone"
        withSubTitle $ do
                       setText "MotionSensor"
                       Plot.setFontSize 12
        setPlots 1 1
        withPlot (1,1) $ do
                         setDataset (Line, (VS.fromList x_s), [(VS.fromList y_s)])
                         addAxis XAxis (Side Lower) $ do
                                                      setGridlines Major True
                                                      withAxisLabel $ setText "time (s)"
                         addAxis YAxis (Side Lower) $ do
                                                      setGridlines Major True
                                                      withAxisLabel $ setText "amplitude (α)"
                         setRangeFromData XAxis Lower Linear
                         setRangeFromData YAxis Lower Linear

