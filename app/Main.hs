{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE BangPatterns, BinaryLiterals, ConstraintKinds,
DataKinds,DefaultSignatures,DeriveDataTypeable,DeriveFoldable,DeriveFunctor,DeriveGeneric,
DeriveTraversable,DoAndIfThenElse,EmptyDataDecls,ExistentialQuantification,FlexibleContexts,
FlexibleInstances,FunctionalDependencies,GADTs,GeneralizedNewtypeDeriving,InstanceSigs,KindSignatures,
LambdaCase,MultiParamTypeClasses,MultiWayIf,NamedFieldPuns,
OverloadedStrings,PartialTypeSignatures,PatternGuards,PolyKinds,RankNTypes,RecordWildCards,
ScopedTypeVariables,StandaloneDeriving,TupleSections,TypeFamilies,TypeSynonymInstances,ViewPatterns #-}

module Main where

import GI.Cairo 
import Graphics.Rendering.Cairo as Cairo
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Plot as Plot
import Foreign.Ptr (castPtr)

import Control.Monad
import Control.Monad.Trans.Reader

import Data.Maybe
import Data.Monoid ((<>))
import Data.Binary.Put

import Data.GI.Base
import GI.GLib.Functions as GLIB
import GI.Gtk.Objects.Builder
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Objects
import qualified GI.Gtk.Objects.Window
import GI.Gtk hiding (main)

import Data.Time as Time
import Data.Time.Clock
import System.Hardware.Serialport
import System.IO

import UartInterface
import PID_Optimization

import qualified RIO.Text as T
import qualified RIO.Vector.Storable as VS
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import RIO.Directory
import RIO.FilePath
import RIO hiding (on)

type Channel = IORef ([Double], [Double])
newChannel = newIORef ([0.1], [0.1]) :: IO (Channel)
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

rs :: UTCTime -> Uart -> [Channel] -> IO (Bool)
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

  return True

activateRender :: (Int, Int) -> (Channel, DrawingArea) -> IO ()
activateRender (width, height) (channel, cairoArea) = do
  widgetSetSizeRequest cairoArea (fromIntegral width) (fromIntegral height)
  on cairoArea #draw $ \context -> do
    values <- readChannel channel
    startTime <- Time.getCurrentTime
    let !rendered = Plot.render ((uncurry figure) values) (width, height) 
    renderWithContext context rendered
    stopTime <- Time.getCurrentTime
    return True
  return ()

selectFile :: Window -> Entry -> IO()
selectFile mainWindow displayEntry = do
  dialog <- new FileChooserDialog [ #title := "Select File"
                                  , #action := FileChooserActionSelectFolder
                                  , #createFolders := True]
  _ <- dialogAddButton dialog "gtk-cancel" $ (toEnum . fromEnum) ResponseTypeCancel
  _ <- dialogAddButton dialog "gtk-open" $ (toEnum . fromEnum )ResponseTypeAccept
  widgetShow dialog
  response <- dialogRun dialog
  when (response == (toEnum . fromEnum) ResponseTypeAccept) $
    fileChooserGetFilename dialog >>= (\t -> setEntryText displayEntry $ (T.pack . fromJust) t)
  widgetDestroy dialog

interleaveLists :: [a] -> [a] -> [a]
interleaveLists xs     []     = []
interleaveLists []     ys     = []
interleaveLists (x:xs) (y:ys) = x : y : interleaveLists xs ys

saveChannelToFile :: Channel -> FilePath -> IO()
saveChannelToFile channel file = do
  (times, values) <- readChannel channel
  let joinedlist = interleaveLists times values
  BL.writeFile file $ runPut (mapM_ putDoublele joinedlist)
  return ()
  
saveChannelsToFile :: [Channel] -> UTCTime -> FilePath -> IO()
saveChannelsToFile channels startTime path = do
  exist <- doesDirectoryExist path
  let fileNameBase = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) startTime
      fileNames = [path </> fileNameBase ++ "_" ++ show i | i <- [1..3]]
      channelsWithFileNames = zipWith (,) channels fileNames 
  print $ fileNames 
  case (exist && (path /= "")) of
    True -> mapM_ (uncurry saveChannelToFile) channelsWithFileNames
    False -> return ()

main :: IO ()
main = do
  Gtk.init Nothing
  builder <- builderNewFromFile "baseStation.glade"
  mainWindow <- getObject builder "mainWindow" Window
  toplevelBox <- getObject builder "topLevelBox" Gtk.Box
  pEntry <- getObject builder "pEntry" Entry
  dEntry <- getObject builder "dEntry" Entry
  iEntry <- getObject builder "iEntry" Entry
  sendBtn <- getObject builder "sendBtn" Button
  directoryEntry <- getObject builder "directoryEntry" Entry
  savePlotContentBtn <- getObject builder "savePlotContentBtn" Button
  selectFolderBtn <- getObject builder "selectFolderBtn" Button
    
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
        print iParam
        bangSerPort serport pParam dParam iParam
        return ()

  on sendBtn #clicked $ sendParams
  on selectFolderBtn #clicked $ selectFile mainWindow directoryEntry
  on savePlotContentBtn #clicked $ (T.unpack <$> getEntryText directoryEntry)
                                   >>= saveChannelsToFile channels startTime 
  
  on mainWindow #destroy $ do
    closeUart serport
    mainQuit
    
  on mainWindow #keyPressEvent $ \event -> do
    name <- event `get` #keyval >>= Gdk.keyvalName
    when (name == Just "Escape") mainQuit
    return False
  
  GLIB.timeoutAdd 0 8 $ rs startTime serport channels
  GLIB.timeoutAdd 0 200 $ ( mapM_ widgetQueueDraw cairoareas >> return True)
  
  #showAll mainWindow
  Gtk.main

figure :: [Double] -> [Double] -> Figure()
figure x_s y_s = do
        withLineDefaults $ Plot.setLineWidth 0.2
        withTextDefaults $ setFontFamily "Cantarell"
        withTitle $ Plot.setFontSize 12  >> setText "Drone"
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

