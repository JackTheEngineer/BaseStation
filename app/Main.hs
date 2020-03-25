{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Main where

import Control.Monad.Trans.Reader (runReaderT)
import qualified GI.Cairo as GI.Cairo
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))
import Foreign.Ptr (castPtr)

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

import Data.Text hiding (filter, map, length)
import Data.Maybe
import Data.Monoid ((<>))
import GI.Gtk.Objects.Builder
import Data.GI.Base
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk 
import GI.Gtk
       (widgetShowAll
       , mainQuit
       , onWidgetDestroy
       , onButtonClicked
       , Button(..)
       , Window(..)
       , DrawingArea(..)
       , Entry(..)
       , Label(..)
       , builderGetObject
       , builderAddFromFile
       , builderNew
       , widgetGetAllocatedWidth
       , widgetGetAllocatedHeight
       , widgetSetSizeRequest)

import GI.GLib.Functions
import GI.Gtk.Objects.Entry
import GI.Gtk.Objects.EntryBuffer

import Data.Time
import Data.Time.Clock

import System.IO
import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart.Backend.Cairo
import Control.Lens
import System.Hardware.Serialport

import UartInterface
import PID_Optimization


-- | This function bridges gi-cairo with the hand-written cairo
-- package. It takes a `GI.Cairo.Context` (as it appears in gi-cairo),
-- and a `Render` action (as in the cairo lib), and renders the
-- `Render` action into the given context.
renderWithContext :: GI.Cairo.Context -> Render (a) -> IO (a)
renderWithContext ct r = withManagedPtr ct $ \p ->
                         runReaderT (runRender r) (Cairo (castPtr p))

getObject b name cast = builderGetObject b name >>= unsafeCastTo cast . fromJust

mapTuple f (a, b) = (f a, f b)

quaternionWellFormed q = ((q0 q) > 0.0) && ((q0 q) <= 1.0)

readQuat :: SerialPort -> TVar [(UTCTime, Quaternion)] -> Label -> IO(Bool)
readQuat serport quatlist label = do
  quat <- getAndDecodeQuaternion serport
  -- case quat of
  --   Just q -> do
  --     ct <- Data.Time.getCurrentTime
  --     l <- atomically $ readTVar quatlist
  --     atomically $ writeTVar quatlist ((ct, q):l)
  --     let qs = filter (\x -> (quaternionWellFormed (snd x))) l
  --         ll = length qs
  --     if ll >= 2 then do
  --       let timeDiff = realToFrac $ diffUTCTime (fst (l!!0)) (fst (l!!(ll-1)))
  --           angles = map ((*) (180.0/pi)) $ quaternionToEuler (q0 q) (q1 q) (q2 q) (q3 q)
  --       Gtk.labelSetText label (pack (show (calculateError (map snd qs) timeDiff)))
  --       print $ "Euler Angles: " ++ show angles
  --       else do
  --       return ()
  --   Nothing -> do
  --     return ()
  return True

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
  widgetSetSizeRequest canvas (fst sz) (snd sz)

  on canvas #draw $ \context -> do
    renderWithContext context (runBackend (defaultEnv bitmapAlignmentFns) (render chart szd))
    return True

  serport <- openUart "/dev/ttyACM0"
  
  let sendParams = do
        pParam <- (read . unpack) <$> getEntryText pEntry :: IO(Float)
        dParam <- (read . unpack) <$> getEntryText dEntry :: IO(Float)
        iParam <- (read . unpack) <$> getEntryText iEntry :: IO(Float)
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
    -- when (name == Just "Return") sendParams
    return False

  quaternionList <- newTVarIO []
  on sendBtn #clicked $ sendParams
  on clearErrBtn #clicked $ do
    atomically $ writeTVar quaternionList []

  id <- timeoutAdd 0 20 $ readQuat serport quaternionList errLabel  
  #showAll mainWindow
  Gtk.main



setLinesBlue :: PlotLines a b -> PlotLines a b
setLinesBlue = plot_lines_style  . line_color .~ opaque blue

chart :: Renderable ()
chart = toRenderable layout
  where
    am :: Double -> Double
    am x = (sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))

    sinusoid1 = plot_lines_values .~ [[ (x,(am x)) | x <- [0,(0.5)..400]]]
              $ plot_lines_style  . line_color .~ opaque blue
              $ plot_lines_title .~ "am"
              $ def

    sinusoid2 = plot_points_style .~ filledCircles 2 (opaque red)
              $ plot_points_values .~ [ (x,(am x)) | x <- [0,7..400]]
              $ plot_points_title .~ "am points"
              $ def

    layout = layout_title .~ "Amplitude Modulation"
           $ layout_plots .~ [toPlot sinusoid1,
                              toPlot sinusoid2]
           $ def  
