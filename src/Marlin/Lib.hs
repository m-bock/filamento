module Marlin.Lib where

import Linear (V2 (..), V3 (..))
import Marlin.Core
import Marlin.DSL
import Marlin.Math
import Relude

extrudeTo :: V2 Double -> GCode ()
extrudeTo v1 = do
  extrudeSpeed <- getExtrudeSpeed
  linearMove
    & setXY v1
    & setExtrude 10
    & setSpeed extrudeSpeed
    & toGCode

extrude :: Double -> GCode ()
extrude s = do
  extrudeSpeed <- getExtrudeSpeed
  linearMove
    & setExtrude s
    & setSpeed extrudeSpeed
    & toGCode

moveTo :: V2 Double -> GCode ()
moveTo v = do
  speed <- getSpeed

  withRetract $ do
    linearMove
      & setXY v
      & setSpeed speed
      & toGCode

moveZ :: Double -> GCode ()
moveZ z = do
  speed <- getSpeed

  withRetract $ do
    linearMove
      & setZ z
      & setSpeed speed
      & toGCode

getSpeed :: GCode Int
getSpeed = do
  env <- ask
  pure
    $ if env.startLayer == 0
      then env.moveSpeedFirstLayer
      else env.moveSpeed

getExtrudeSpeed :: GCode Int
getExtrudeSpeed = do
  env <- ask
  pure
    $ if env.startLayer == 0
      then env.extrudeSpeedFirstLayer
      else env.extrudeSpeed

moveTo3d :: V3 Double -> GCode ()
moveTo3d v = do
  speed <- getSpeed

  withRetract $ do
    linearMove
      & setXYZ v
      & setSpeed speed
      & toGCode

withRetract :: GCode a -> GCode a
withRetract inner = do
  let retractLength = 3
  extrude (-retractLength)
  ret <- inner
  extrude retractLength
  pure ret

printManyPolyLines :: [[V2 Double]] -> GCode ()
printManyPolyLines = mapM_ printPolyLine

printPolyLine :: [V2 Double] -> GCode ()
printPolyLine [] = pure ()
printPolyLine (v : vs) = do
  moveTo v
  extrudePoints vs

extrudePoints :: [V2 Double] -> GCode ()
extrudePoints vs = do
  forM_ vs $ \v -> do
    extrudeTo v

printSquare :: V2 Double -> V2 Double -> GCode ()
printSquare v1 s = do
  let v2 = v1 + justX s
  let v3 = v2 + justY s
  let v4 = v3 - justX s
  let v5 = v4 - justY s

  printPolyLine [v1, v2, v3, v4, v5]

printTestStripes :: GCode ()
printTestStripes = section "Test Stripes" $ do
  moveTo3d (V3 0 0 0.2)

  let v1 = V2 5.0 20.0
  let v2 = V2 5.0 150.0
  let v3 = V2 10.0 20.0
  let v4 = V2 10.0 150.0

  printManyPolyLines
    [ [v1, v2],
      [v3, v4]
    ]

finalPark :: GCode ()
finalPark = do
  env <- ask

  extrude (-3)
  moveTo3d env.parkingPosition

homeOrResume :: GCode ()
homeOrResume = do
  env <- ask

  if env.startLayer == 0
    then do
      section "autoHome" $ do
        autoHome
          & setSkipIfTrusted True
          & toGCode
    else do
      section "Resume" $ do
        setPosition
          & setXYZ env.parkingPosition
          & toGCode

initPrinter :: GCode a -> GCode a
initPrinter inner = do
  env <- ask

  setUnits Millimeter

  setExtruderRelative

  heatup homeOrResume

  printTestStripes

  ret <- inner

  finalPark

  pure ret

heatup :: GCode a -> GCode a
heatup inner = do
  env <- ask
  section "Heatup" $ do
    setBedTemperature
      & setTargetTemperature env.bedTemperature
      & toGCode

    setHotendTemperature
      & setTargetTemperature env.hotendTemperature
      & toGCode

  ret <-
    section "Prepare while waiting" $ do
      inner

  section "Wait for temperatures" $ do
    waitForBedTemperature
      & setTargetTemperature env.bedTemperature
      & toGCode

    waitForHotendTemperature
      & setTargetTemperature env.hotendTemperature
      & toGCode

  pure ret

redefineOriginFromParking :: GCode ()
redefineOriginFromParking = do
  env <- ask

  let (V3 parkX parkY _) = env.parkingPosition

  let v = V2 parkX parkY

  setPosition
    & setXY v
    & toGCode

getOriginVec :: GCode (V2 Double)
getOriginVec = do
  env <- ask

  let (V3 bedX bedY _) = env.printSize
  let (V3 sketchX sketchY _) = env.sketchSize

  let x = -((bedX / 2) - (sketchX / 2))
  let y = -((bedY / 2) - (sketchY / 2))

  let v = V2 x y
  pure v