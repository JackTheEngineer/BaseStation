{-# LANGUAGE OverloadedStrings #-}
module UartInterface where 

import System.IO
import System.Hardware.Serialport
import Data.Binary.Get as G
import Data.Binary.Put
import Data.Binary.Strict.BitGet as BG hiding (skip)

import Control.Monad
import Control.Concurrent

import Data.Word
import Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import Data.ByteString.Builder.Extra
import Data.ByteString.Lazy.Internal as BLI
import Data.ByteString.Lazy as BL
import Data.Binary.IEEE754
import Data.Binary.Parser

import PID_Optimization

data StatusCode = StatusCode { rBank :: Word8
                             , rxDataReady :: Word8
                             , txDataSent :: Word8
                             , maxRetransmits :: Word8
                             , rxPipeNum :: Word8
                             , txFifoFull :: Word8 } deriving Show
statusCode = StatusCode <$>
             BG.getAsWord8 1 <*>
             BG.getAsWord8 1 <*>
             BG.getAsWord8 1 <*>
             BG.getAsWord8 1 <*>
             BG.getAsWord8 3 <*>
             BG.getAsWord8 1

data FifoStatus = FifoStatus { _r2 :: Word8
                             , tx_reuse :: Word8
                             , tx_full :: Word8
                             , tx_empty :: Word8
                             , _r1 :: Word8
                             , rx_full :: Word8
                             , rx_empty :: Word8 } deriving Show
fifoStatus = FifoStatus <$>
             BG.getAsWord8 1 <*>
             BG.getAsWord8 1 <*>
             BG.getAsWord8 1 <*>
             BG.getAsWord8 1 <*>
             BG.getAsWord8 2 <*>
             BG.getAsWord8 1 <*>
             BG.getAsWord8 1

data ObserveTx = ObserveTx { packetLostCNT :: Word8
                           , autoRetransmissionCNT :: Word8} deriving Show

observeTx = ObserveTx <$> BG.getAsWord8 4 <*> BG.getAsWord8 4

openUart :: FilePath -> IO(SerialPort)
openUart filepath = do
  serport <- openSerial filepath defaultSerialSettings {commSpeed = CS115200
                                                       , timeout = 3000}
  return serport

bangSerPort serport pGain dGain iGain = do
  let builder =  mconcat [ BB.word8  1
                         , BB.floatLE pGain  -- Kp 
                         , BB.floatLE dGain  -- Kd
                         , BB.floatLE iGain  -- Ki
                         , BB.word8 $ 10 ]
      bs = BL.toStrict $ toLazyByteStringWith (safeStrategy 64 128) BL.empty builder
  numOfSentBytes <- send serport bs
  print $ mconcat ["Bytes Sent: " , show numOfSentBytes]
  
closeUart = closeSerial 

getAndDecodeQuaternion :: SerialPort -> IO (Maybe Quaternion)
getAndDecodeQuaternion serport = do
  bytes <- recv serport 4096
  print $ B.length bytes
  if (B.length bytes) == 32 then do
    return $ Just $ toQuat (G.runGet parseQuat $ fromStrict bytes)
  else do
    return (Nothing)

parseDroneMessage bs = parseDetailLazy (some' getDroneMessage) bs

getDroneMessage = do
  qs <- parseQuat
  skip 16
  endOfLine
  return qs
  
parseQuat = replicateM 4 getFloat32le

quatAsListToEuler l = quaternionToEuler (l!!0) (l!!1) (l!!2) (l!!3)

quaternionToEuler q0 q1 q2 q3 =
  let phi = (atan2 (2*(q0*q1 + q2*q3)) (1 - 2*(q1*q1 + q2*q2)))
      theta = (asin (2*(q0*q2 - q3*q1)))
      psi = (atan2 (2*(q0*q3 + q1*q2)) (1 - 2*(q2*q2 + q3*q3)))
  in [phi, theta, psi]

parseRFM75_StatusCode = do
  status <- G.getByteString 1
  otx <- G.getByteString 1
  fifo <- G.getByteString 1
  msglen <- G.getWord8
  let s = BG.runBitGet status statusCode
      f = BG.runBitGet fifo fifoStatus
      o = BG.runBitGet otx observeTx
  return (msglen, f, o, s)

