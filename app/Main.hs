{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import qualified Data.Text as T
import Linear (V3 (..))
import Linear.V2 (V2 (..))
import Marlin.DSL
import Relude

heatup :: Int -> Int -> GCode a -> GCode a
heatup bedTemp hotendTemp inner = do
  section "Heatup" $ do
    setBedTemperature
      & setTargetTemperature bedTemp
      & toGCode

    setHotendTemperature
      & setTargetTemperature hotendTemp
      & toGCode

  ret <- inner

  section "Wait for temperatures" $ do
    waitForBedTemperature
      & setTargetTemperature bedTemp
      & toGCode

    waitForHotendTemperature
      & setTargetTemperature hotendTemp
      & toGCode

  pure ret

coolDown :: GCode ()
coolDown =
  section "Cool down" $ do
    setHotendOff
    setBedOff
    setFanOff
    motorsOff

initialTestStripes :: GCode ()
initialTestStripes = do
  linearMove
    & setXYZ (V3 5.0 0.0 0.2)
    & setSpeed 10000
    & toGCode

  linearMove
    & setXYZ (V3 5.0 100.0 0.2)
    & setExtrude 10
    & setSpeed 500
    & toGCode

  linearMove
    & setXYZ (V3 10.0 0.0 0.2)
    & setSpeed 10000
    & toGCode

  linearMove
    & setXYZ (V3 10.0 100.0 0.2)
    & setExtrude 10
    & setSpeed 500
    & toGCode

layer :: Int -> GCode ()
layer n = do
  let step = 10.0

  let z = 0.2 + (fromIntegral n * 0.2)

  let extSpeed = if n == 0 then 500 else 1000

  linearMove
    & setZ z
    & setSpeed 10000
    & toGCode

  for_ (enumFromTo 10 20) $ \x -> do
    linearMove
      & setExtrude (-2)
      & toGCode

    linearMove
      & setXY (V2 (fromIntegral x * step) 0.0)
      & setSpeed 10000
      & setExtrude 0
      & toGCode

    linearMove
      & setExtrude (2)
      & toGCode

    linearMove
      & setXY (V2 (fromIntegral x * step) 100.0)
      & setSpeed extSpeed
      & setExtrude 10
      & toGCode

finalUpZ :: GCode ()
finalUpZ = do
  linearMove
    & setX 100.0
    & setY 100.0
    & setSpeed 500
    & toGCode

  linearMove
    & setZ 100.0
    & setSpeed 10000
    & toGCode

sampleProgram :: GCode ()
sampleProgram = do
  setUnits Millimeter
  setExtruderRelative

  heatup 60 200 $ do
    autoHome
      & setSkipIfTrusted True
      & toGCode

  initialTestStripes

  -- layer 1
  -- layer 2
  -- layer 3
  layer 4
  layer 5
  layer 6
  layer 7

  finalUpZ

-- pause 30 -- instead of raw "G4 S30" "wait 30 seconds"
-- coolDown

main :: IO ()
main = do
  let codeStr = toText sampleProgram
  writeFileText "out/myprint.gcode" codeStr
  putStrLn $ T.unpack codeStr