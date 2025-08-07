module Marlin.Lib
  ( extrudeTo,
    extrude,
    moveXY,
    moveZ,
    nextLayer,
    getSpeed,
    getExtrudeSpeed,
    withRetract,
    withZHop,
    printPolyLine,
    extrudePoints,
    printRect,
    getLayerCount,
    printTestStripes,
    finalPark,
    homeOrResume,
    initPrinter,
    readPersistentState,
    filamentChange,
    PersistentState (..),
  )
where

import Data.Aeson (FromJSON, ToJSON, encodeFile)
import Data.Aeson.Decoding (decodeStrict)
import Linear (V2 (..), V3 (..))
import Linear.Metric
import Marlin.Core
import Marlin.DSL
import Marlin.Math
import Relude

extrudeTo :: V2 Double -> GCode ()
extrudeTo v@(V2 x y) = do
  extrudeSpeed <- getExtrudeSpeed

  extrudeLength <- getExtrudeLength v

  gCodeFromCmd
    $ GLinearMove
      gcodeDef
        { x = Just x,
          y = Just y,
          extrude = Just extrudeLength,
          feedrate = Just extrudeSpeed
        }

extrude :: Double -> GCode ()
extrude s = do
  extrudeSpeed <- getExtrudeSpeed
  gCodeFromCmd
    $ GLinearMove
      gcodeDef
        { extrude = Just s,
          feedrate = Just extrudeSpeed
        }

moveXY :: V2 Double -> GCode ()
moveXY (V2 x y) = do
  speed <- getSpeed

  gCodeFromCmd
    $ GLinearMove
      gcodeDef
        { x = Just x,
          y = Just y,
          feedrate = Just speed
        }

moveZ :: Double -> GCode ()
moveZ z = do
  speed <- getSpeed

  gCodeFromCmd
    $ GLinearMove
      gcodeDef
        { z = Just z,
          feedrate = Just speed
        }

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

withRetract :: GCode a -> GCode a
withRetract inner = do
  env <- ask

  gCodeFromCmd
    $ GLinearMove
      gcodeDef
        { extrude = Just (-env.retractLength),
          feedrate = Just env.retractSpeed
        }

  ret <- inner

  gCodeFromCmd
    $ GLinearMove
      gcodeDef
        { extrude = Just env.retractLength,
          feedrate = Just env.retractSpeed
        }

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

printPolyLine :: [V2 Double] -> GCode ()
printPolyLine [] = pure ()
printPolyLine (v : vs) = do
  moveXY v
  extrudePoints vs

extrudePoints :: [V2 Double] -> GCode ()
extrudePoints vs = do
  forM_ vs $ \v -> do
    extrudeTo v

printRect :: V2 Double -> V2 Double -> GCode ()
printRect v1 s = do
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

printTestStripes :: GCode ()
printTestStripes = section "Test Stripes" $ do
  moveZ 0.2

  -- section "Thick test stripe" do
  --   moveTo (V2 10.0 5.0)
  --   extrude 5
  --   extrudeTo (V2 215.0 5.0)
  --   extrude (-1)

  section "Thin test stripe" do
    moveXY (V2 10.0 10.0)
    extrude 5
    extrudeTo (V2 215.0 10.0)
    extrude (-1)

-- raw "G1 Z0.2 F1200" "Move to first layer height"
-- raw "G1 X10 Y5 F3000" "Move to start position"
-- raw "G1 E5 F500" "Prime nozzle"
-- raw "G1 X215 Y5 E15 F600" "Draw a long test stripe"
-- raw "G1 E-1 F300" "Retract a bit"
-- raw "G1 Z1.0 F1200" "Lift nozzle to avoid dragging"
-- updatePos (fmap Just $ V3 215.0 5.0 1.0)

-- withRetract $ moveTo (V2 10.0 10.0)

-- moveZ 0.2

-- printPolyLine [V2 10.0 10.0, V2 215.0 10.0]

finalPark :: GCode ()
finalPark = do
  env <- ask

  let V3 parkX parkY parkZ = env.parkingPosition

  extrude (-3)

  moveZ parkZ
  moveXY (V2 parkX parkY)

homeOrResume :: GCode ()
homeOrResume = do
  st <- get

  if st.currentLayer == 0
    then do
      section "autoHome" $ do
        gCodeFromCmd $ GAutoHome gcodeDef
    else do
      section "Resume" $ do
        gCodeFromCmd $ GSetPosition gcodeDef

initPrinter :: GCode a -> GCode a
initPrinter inner = do
  env <- ask

  setUnits Millimeter

  setExtruderRelative

  gCodeFromCmd
    $ GLinearMove
      gcodeDef
        { x = Just 0,
          y = Just 0,
          z = Just 0
        }

  heatup homeOrResume

  do
    moveXY env.transpose
    gCodeFromCmd
      $ GSetPosition
        gcodeDef
          { x = Just 0,
            y = Just 0
          }

  do
    beep
    moveXY (V2 (-1) 0)
    pause 10

  printTestStripes

  ret <- inner

  finalPark

  pure ret

heatup :: GCode a -> GCode a
heatup inner = do
  env <- ask
  section "Heatup" $ do
    gCodeFromCmd
      $ MSetBedTemperature
        gcodeDef
          { degrees = Just env.bedTemperature
          }

    gCodeFromCmd
      $ MSSetHotendTemperature
        gcodeDef
          { degrees = Just env.hotendTemperature
          }

  ret <-
    section "Prepare while waiting" $ do
      inner

  section "Wait for temperatures" $ do
    gCodeFromCmd
      $ MWaitForBedTemperature
        gcodeDef
          { degrees = Just env.bedTemperature
          }

    gCodeFromCmd
      $ MWaitForHotendTemperature
        gcodeDef
          { degrees = Just env.hotendTemperature
          }

  pure ret

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

    local (\env -> env {extrudeSpeed = 200}) $ do
      extrude 10

    local (\env -> env {extrudeSpeed = 800}) $ do
      extrude 200

    local (\env -> env {extrudeSpeed = 200}) $ do
      extrude 50
      extrude (-1)

    beep

    local (\env -> env {extrudeSpeed = 200}) $ do
      extrude (-1)
      extrude 1

    beep

beep :: GCode ()
beep = do
  gCodeFromCmd
    $ MPlayTone
      gcodeDef
        { frequency = Just 1000,
          milliseconds = Just 500
        }

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

-------

changeColor :: Text -> GCode ()
changeColor = undefined

purge :: GCode ()
purge = undefined

type FilamentDef = [(Text, Double)]

getFilamentDef :: GCode a -> FilamentDef
getFilamentDef = undefined

printFilamentDef :: FilamentDef -> GCode ()
printFilamentDef = undefined