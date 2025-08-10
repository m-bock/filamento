module Filamento.Lib
  ( extrudeXY,
    extrude,
    moveXY,
    moveZ,
    withRetract,
    withZHop,
    printPolyLine,
    extrudePoints,
    printRect2d,
    printRect,
    printTestStripes,
    finalPark,
    homeOrResume,
    initPrinter,
    filamentChange,
    nextLayer,
  )
where

import Data.Convertible
import Filamento.Classes
import Filamento.Core
import Filamento.Math hiding (justX, justY)
import Filamento.Types
import Linear (V2 (..), V3 (..))
import Relude

nextLayer :: GCode ()
nextLayer = do
  env <- ask
  st <- get

  let z = fromMm (toMm env.firstLayerHeight + convert st.currentLayer * toMm env.layerHeight)

  modify \st -> st {currentLayer = st.currentLayer + 1}

  moveToZ z

withRetract :: GCode a -> GCode a
withRetract inner = section "retract" do
  env <- ask

  extrude (fromMmPerSec 2000) (-env.retractLength)

  ret <- inner

  extrude (fromMmPerSec 2000) env.retractLength

  pure ret

withZHop :: GCode a -> GCode a
withZHop inner = section "zHop" do
  st <- get
  env <- ask
  let V3 _ _ z = toMm st.currentPosition
  moveToZ (addDelta (fromMm z) env.zHop)
  ret <- inner
  moveToZ (fromMm z)
  pure ret

printPolyLine :: [Position3D] -> GCode ()
printPolyLine [] = pure ()
printPolyLine (v : vs) = do
  moveToXYZ v
  extrudePoints vs

extrudePoints :: [Position3D] -> GCode ()
extrudePoints vs = do
  forM_ vs $ \v -> do
    extrudeToXYZ v

printRect2d :: Position2D -> Delta2D -> GCode ()
printRect2d (toMm -> V2 x y) delta = do
  (toMm -> V3 _ _ z) <- getCurrentPosition
  let pos = fromMm $ V3 x y z
  printRect pos delta

printRect :: Position3D -> Delta2D -> GCode ()
printRect v1 (toMm -> V2 dx dy) = do
  let dlt3 = fromMm $ V3 dx dy 0
  let v2 = addDelta v1 (justX dlt3)
  let v3 = addDelta v2 (justY dlt3)
  let v4 = subDelta v3 (justX dlt3)
  let v5 = subDelta v4 (justY dlt3)

  printPolyLine [v1, v2, v3, v4, v5]

printTestStripes :: GCode ()
printTestStripes = section "Test Stripes" $ do
  moveToZ (fromMm 0.2)

  -- section "Thick test stripe" do
  --   moveTo (V2 10.0 5.0)
  --   extrude 5
  --   extrudeTo (V2 215.0 5.0)
  --   extrude (-1)

  section "stripe 1" do
    moveToXY (fromMm $ V2 5 5)
    extrude (fromMmPerSec 2000) 5
    extrudeToXY (fromMm $ V2 215.0 5)
    extrude (fromMmPerSec 2000) (-1)

  section "stripe 2" do
    withRetract $ withZHop $ moveToXY (fromMm $ V2 5 10)
    extrudeToXY (fromMm $ V2 215.0 10)

-- raw "G1 Z0.2 F1200" "Move to first layer height"
-- raw "G1 X10 Y5 F3000" "Move to start pos"
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

  let V3 parkX parkY parkZ = toMm env.parkingPosition

  extrude (fromMmPerSec 2000) (-3)

  moveZ (fromMm parkZ)
  moveToXY (fromMm $ V2 parkX parkY)

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

cleaningOpportunity :: GCode ()
cleaningOpportunity = section "Cleaning Opportunity" do
  moveToXYZ (fromMm $ V3 0 0 2)
  playTone_
  pause (fromSecs 10)

initPrinter :: GCode a -> GCode a
initPrinter inner = do
  setUnits Millimeter

  setExtruderRelative

  moveToXYZ (fromMm $ V3 0 0 0)

  heatup homeOrResume

  cleaningOpportunity

  printTestStripes

  ret <- inner

  finalPark

  pure ret

heatup :: GCode a -> GCode a
heatup inner = do
  env <- ask
  setBedTemperature env.bedTemperature
  setHotendTemperature env.hotendTemperature

  ret <- inner

  do
    waitForBedTemperature env.bedTemperature
    waitForHotendTemperature env.hotendTemperature

  pure ret

printPolygon :: Int -> Position3D -> Distance -> GCode ()
printPolygon n v s'
  | n < 3 = pure () -- Polygons need at least 3 sides
  | s' <= 0 = pure () -- Side length must be positive
  | otherwise = do
      let angle = 2 * pi / fromIntegral n
          s = toMm s'
          points =
            [ addDelta
                v
                (fromMm $ V3 (cos (angle * fromIntegral i) * s) (sin (angle * fromIntegral i) * s) 0)
              | i <- [0 .. n - 1]
            ]
      case viaNonEmpty head points of
        Nothing -> pure ()
        Just firstPoint -> printPolyLine (points ++ [firstPoint])

filamentChange :: GCode ()
filamentChange = do
  section "Filament Change" $ do
    finalPark

    playTone_

    raw "M0" "Pause for filament change"

    extrude (fromMmPerSec 2000) 5

    pause (fromSecs 2)

    local (\env -> env {extrudeSpeed = fromMmPerSec 200}) $ do
      extrude (fromMmPerSec 2000) 10

    local (\env -> env {extrudeSpeed = fromMmPerSec 800}) $ do
      extrude (fromMmPerSec 2000) 200

    local (\env -> env {extrudeSpeed = fromMmPerSec 200}) $ do
      extrude (fromMmPerSec 2000) 50
      extrude (fromMmPerSec 2000) (-1)

    playTone_

    local (\env -> env {extrudeSpeed = fromMmPerSec 200}) $ do
      extrude (fromMmPerSec 2000) (-1)
      extrude (fromMmPerSec 2000) 1

    playTone_

-------

purge :: GCode ()
purge = undefined

type FilamentDef = [(Text, Double)]

getFilamentDef :: GCode a -> FilamentDef
getFilamentDef = undefined

printFilamentDef :: FilamentDef -> GCode ()
printFilamentDef = undefined