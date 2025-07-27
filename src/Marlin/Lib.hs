module Marlin.Lib where

import Linear (V2 (..), V3 (..))
import Linear.Metric
import Linear.V (V)
import Marlin.Core
import Marlin.DSL
import Marlin.Math
import Relude

extrudeTo :: V2 Double -> GCode ()
extrudeTo v1 = do
  extrudeSpeed <- getExtrudeSpeed
  extrudeLength <- getExtrudeLength v1
  linearMove
    & setXY v1
    & setExtrude extrudeLength
    & setSpeed extrudeSpeed
    & toGCode

extrude :: Double -> GCode ()
extrude s = do
  extrudeSpeed <- getExtrudeSpeed
  linearMove
    & setExtrude s
    & setSpeed extrudeSpeed
    & toGCode

getExtrudeLength :: V2 Double -> GCode Double
getExtrudeLength v = do
  extrudeMM <- getExtrudeMM
  st <- get
  let V3 curX curY _ = st.currentPosition
  let lineLength = distance (V2 curX curY) v
  pure (lineLength * extrudeMM)

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
  withRetract $ moveZDirectly z

moveZDirectly :: Double -> GCode ()
moveZDirectly z = do
  speed <- getSpeed

  linearMove
    & setZ z
    & setSpeed speed
    & toGCode

nextLayer :: GCode ()
nextLayer = do
  st <- get
  env <- ask
  let newLayer = st.currentLayer + 1
  put $ st {currentLayer = newLayer}
  moveZ (env.layerHeight * fromIntegral newLayer)

getSpeed :: GCode Int
getSpeed = do
  b <- isFirstLayers
  env <- ask
  pure
    $ if b
      then env.moveSpeedFirstLayer
      else env.moveSpeed

getExtrudeSpeed :: GCode Int
getExtrudeSpeed = do
  b <- isFirstLayers
  env <- ask
  pure
    $ if b
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

  -- extrude (-5)

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
  st <- get
  env <- ask

  if st.currentLayer == 0
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

isFirstLayers :: GCode Bool
isFirstLayers = do
  st <- get
  let (V3 _ _ z) = st.currentPosition
  pure (z <= 0.3)

getExtrudeMM :: GCode Double
getExtrudeMM = do
  env <- ask
  let vPerMm = env.layerHeight * env.lineWidth
      aFil = pi * (env.filamentDia ^ 2) / 4
  pure (vPerMm / aFil)

printPolygon :: Int -> V2 Double -> Double -> GCode ()
printPolygon n v s
  | n < 3 = pure () -- Polygons need at least 3 sides
  | s <= 0 = pure () -- Side length must be positive
  | otherwise = do
      let angle = 2 * pi / fromIntegral n
          points = [v + V2 (s * cos (angle * fromIntegral i)) (s * sin (angle * fromIntegral i)) | i <- [0 .. n - 1]]
      case viaNonEmpty head points of
        Nothing -> pure ()
        Just firstPoint -> printPolyLine (points ++ [firstPoint])