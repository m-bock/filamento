module Marlin.Lib
  ( extrudeXY,
    extrude,
    moveXY,
    moveZ,
    nextLayer,
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
import Marlin.DSL
import Marlin.Math
import Relude

nextLayer :: GCode ()
nextLayer = do
  st <- get
  env <- ask
  let newLayer = st.currentLayer + 1
  put $ st {currentLayer = newLayer}
  moveZ (env.layerHeight * fromIntegral newLayer)

withRetract :: GCode a -> GCode a
withRetract inner = do
  env <- ask

  extrude (-env.retractLength)

  ret <- inner

  extrude env.retractLength

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
    extrudeXY v

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
    extrudeXY (V2 215.0 10.0)
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
  env <- ask
  st <- get

  if st.currentLayer == 0
    then do
      section "autoHome" $ do
        autoHome
    else do
      section "Resume" $ do
        setPositionXYZ env.parkingPosition

initPrinter :: GCode a -> GCode a
initPrinter inner = do
  env <- ask

  setUnits Millimeter

  setExtruderRelative

  moveXYZ (V3 0 0 0)

  heatup homeOrResume

  do
    moveXY env.transpose
    setPositionXY (V2 0 0)

  do
    playTone_
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
    setBedTemperature env.bedTemperature
    setHotendTemperature env.hotendTemperature

  ret <-
    section "Prepare while waiting" $ do
      inner

  section "Wait for temperatures" $ do
    waitForBedTemperature env.bedTemperature
    waitForHotendTemperature env.hotendTemperature

  pure ret

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

    playTone_

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

    playTone_

    local (\env -> env {extrudeSpeed = 200}) $ do
      extrude (-1)
      extrude 1

    playTone_

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