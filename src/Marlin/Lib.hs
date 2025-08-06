module Marlin.Lib where

import Data.Aeson (FromJSON, ToJSON, encodeFile)
import Data.Aeson.Decoding (decodeStrict)
import Linear (V2 (..), V3 (..))
import Linear.Metric
import Linear.V (V, _V)
import Linear.Vector ((^*))
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

extrudeToFinal :: V2 Double -> GCode ()
extrudeToFinal v1 = do
  extrudeSpeed <- getExtrudeSpeed

  st <- get

  let V3 curX curY _ = st.currentPosition

  let cur = V2 curX curY

  let diff = v1 - cur

  let v' = shortenVecBy diff (pure 3)

  let v'' = cur + v'

  extrudeLength <- getExtrudeLength v''

  linearMove
    & setXY v''
    & setExtrude extrudeLength
    & setSpeed extrudeSpeed
    & toGCode

  linearMove
    & setXY v1
    & setSpeed extrudeSpeed
    & toGCode

shortenVecBy :: V2 Double -> V2 Double -> V2 Double
shortenVecBy v amount =
  let len = norm v
      d = norm amount
   in if len > d
        then v - normalize v ^* d
        else v

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

  linearMove
    & setXY v
    & setSpeed speed
    & toGCode

-- moveZ :: Double -> GCode ()
-- moveZ z = do
--   st <- get
--   let V3 _ _ curZ = st.currentPosition
--   unless (curZ == z) do
--     withRetract $ moveZDirectly z

moveZ :: Double -> GCode ()
moveZ z = do
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
  env <- ask

  linearMove
    & setExtrude (-env.retractLength)
    & setSpeed env.retractSpeed
    & toGCode

  ret <- inner

  linearMove
    & setExtrude env.retractLength
    & setSpeed env.retractSpeed
    & toGCode

  pure ret

withZHop :: GCode a -> GCode a
withZHop inner = do
  st <- get
  env <- ask
  let V3 _ _ z = st.currentPosition
  moveZ (z + env.zHop)
  ret <- inner
  moveZ z
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

getLayerCount :: GCode Int
getLayerCount = do
  env <- ask
  let V3 _ _ sketchZ = env.sketchSize
  pure (floor (sketchZ / env.layerHeight))

printTestStripesLikeNeptune :: GCode ()
printTestStripesLikeNeptune = section "Test Stripes" $ do
  -- raw "G28" "Home all axes"
  raw "G92 E0" "Reset extruder"
  raw "G1 Z0.2 F1200" "Move to first layer height"
  raw "G1 X10 Y5 F3000" "Move to start position"
  raw "G1 E5 F500" "Prime nozzle"
  raw "G1 X215 Y5 E15 F600" "Draw a long test stripe"
  raw "G1 E-1 F300" "Retract a bit"
  raw "G1 Z1.0 F1200" "Lift nozzle to avoid dragging"
  updatePos (fmap Just $ V3 215.0 5.0 1.0)

  withRetract $ moveTo (V2 10.0 10.0)

  moveZ 0.2

  printManyPolyLines
    [ [V2 10.0 10.0, V2 215.0 10.0]
    ]

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
          --   & setSkipIfTrusted True
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

  setPosition & setXYZ (V3 0 0 0) & toGCode

  heatup homeOrResume

  do
    moveTo env.transpose
    setPosition & setXY (V2 0 0) & toGCode

  do
    beep
    moveTo (V2 (-1) 0)
    pause 10

  printTestStripesLikeNeptune

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
  pure (z <= 0.4)

getExtrudeLength :: V2 Double -> GCode Double
getExtrudeLength v = do
  extrudeMM <- getExtrudeMM
  st <- get
  let V3 curX curY _ = st.currentPosition
  let lineLength = distance (V2 curX curY) v
  pure (lineLength * extrudeMM)

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

filamentChange :: GCode ()
filamentChange = do
  section "Filament Change" $ do
    finalPark

    beep

    raw "M0" "Pause for filament change"

    extrude 5

    pause 2

    linearMove
      & setSpeed 200
      & setExtrude 50
      & toGCode

    linearMove
      & setSpeed 800
      & setExtrude 200
      & toGCode

    linearMove
      & setSpeed 200
      & setExtrude 50
      & toGCode

    beep

    linearMove
      & setSpeed 200
      & setExtrude (-1)
      & toGCode

    linearMove
      & setSpeed 200
      & setExtrude 1
      & toGCode

    beep

-- moveTo3d prevPosition

beep :: GCode ()
beep = do
  playTone
    & setFrequency 500
    & setDuration 500
    & toGCode

data PersistentState = PersistentState
  {count :: Int}
  deriving (Show, Eq, Generic)

instance FromJSON PersistentState

instance ToJSON PersistentState

readPersistentState :: IO PersistentState
readPersistentState = do
  let persistentFile = "persistent-state.json"
  c <- readFileBS persistentFile
  v <- case decodeStrict c of
    Just x -> pure x
    Nothing -> error "Failed to decode printing-state.json"

  let v' = v {count = v.count + 1}
  encodeFile persistentFile v'

  pure v
