{-# OPTIONS_GHC -Wno-type-defaults #-}

module Sketch01 where

import qualified Data.Text as T
import Linear (V3 (..))
import Linear.V2 (V2 (..))
import Marlin.DSL
import Marlin.Lib
import Relude

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
    & setXYZ (V3 (-5.0) 0.0 0.2)
    & setSpeed 10000
    & toGCode

  linearMove
    & setXYZ (V3 (-5.0) 100.0 0.2)
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

  let extSpeed = if n == 0 then 500 else 3000

  linearMove
    & setZ z
    & setSpeed 10000
    & toGCode

  let startX = 14
  let nStripes = 5

  for_ (enumFromTo startX (startX + nStripes - 1)) $ \x -> do
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

parkPosition :: V3 Double
parkPosition = V3 100.0 100.0 100.0

resume :: GCode ()
resume =
  section "Resume" $ do
    setPosition
      & setXYZ parkPosition
      & toGCode

-- raw "G92 X0 Y0 Z10 E123.4"

home :: GCode ()
home =
  section "Home" $ do
    autoHome
      & setSkipIfTrusted True
      & toGCode

redefineOrigin :: V2 Double -> V2 Double -> GCode ()
redefineOrigin (V2 bedX bedY) (V2 sketchX sketchY) = do
  let x = (bedX / 2) - (sketchX / 2)
  let y = (bedY / 2) - (sketchY / 2)

  linearMove
    & setXY (V2 x y)
    & setZ 0.2
    & toGCode

  setPosition
    & setXY (V2 0.0 0.0)
    & setZ 0.2
    & toGCode

sampleProgram :: GCode ()
sampleProgram = do
  let shouldResume = False

  setUnits Millimeter
  setExtruderRelative

  heatup $ do
    if shouldResume then resume else home

  initialTestStripes

  let bedSize = V2 225.0 225.0
  let sketchSize = V2 100.0 100.0

  forM_ [14 .. 17] $ \n -> do
    section ("Layer " <> show n) $ layer n

  finalPark

-- pause 30 -- instead of raw "G4 S30" "wait 30 seconds"
-- coolDown

constMaxMoveSpeed :: Int
constMaxMoveSpeed = 10000

constMaxExtrudeSpeed :: Int
constMaxExtrudeSpeed = 3000

main :: IO ()
main = do
  let codeStr = toText sampleProgram
  writeFileText "out/myprint.gcode" codeStr
  putStrLn $ T.unpack codeStr