module Filamento.Lib
  ( extrude,
    moveBy,
    moveByZ,
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
    printLayers,
    printLayers_,
    printSketchFrame,
    getFilamentDef,
  )
where

import Filamento.Classes
import Filamento.Core
import Filamento.Types
import Linear (V2 (..), V3 (..))
import Relude

printLayers :: (OutOf -> GCode ()) -> GCode ()
printLayers printLayer = do
  env <- ask
  let V3 _ _ sketchHeight = toMm env.sketchSize
  let countLayers = round ((sketchHeight - toMm env.firstLayerHeight) / toMm env.layerHeight)
  forM_ [0 .. countLayers - 1] $ \i -> section ("layer " <> show i) do
    let outOf = fromInt (i, countLayers)

    nextLayer
    printLayer outOf

printLayers_ :: GCode () -> GCode ()
printLayers_ gcode = printLayers (const gcode)

printSketchFrame :: GCode ()
printSketchFrame = section "sketchFrame" do
  env <- ask
  let size2d = delta2From3 env.sketchSize
  let centerBed = addDelta mempty (scale 0.5 env.bedSize - scale 0.5 size2d)
  printRect2d centerBed size2d

withRetract :: GCode a -> GCode a
withRetract inner = section "retract" do
  env <- ask

  extrude (fromMmPerSec 2000) (-env.retractLength)

  ret <- inner

  extrude (fromMmPerSec 2000) env.retractLength

  pure ret

withZHop :: GCode a -> GCode a
withZHop inner = section "zHop" do
  st <- gcodeStateGet
  env <- ask
  let V3 _ _ z = toMm st.currentPosition
  moveToZ (addDelta (fromMm z) env.zHop)
  ret <- inner
  moveToZ (fromMm z)
  pure ret

printPolyLine :: [Position3D] -> GCode ()
printPolyLine [] = pure ()
printPolyLine (v : vs) = do
  moveTo v
  extrudePoints vs

extrudePoints :: [Position3D] -> GCode ()
extrudePoints vs = do
  forM_ vs $ \v -> do
    extrudeTo v

printRect2d :: Position2D -> Delta2D -> GCode ()
printRect2d (toMm -> V2 x y) delta = do
  (toMm -> V3 _ _ z) <- getCurrentPosition
  let pos = fromMm $ V3 x y z
  printRect pos delta

printRect :: Position3D -> Delta2D -> GCode ()
printRect v1 (toMm -> V2 dx dy) = section "printRect" do
  let dlt3 = fromMm $ V3 dx dy 0
  let v2 = addDelta v1 (justX dlt3)
  let v3 = addDelta v2 (justY dlt3)
  let v4 = subDelta v3 (justX dlt3)
  let v5 = subDelta v4 (justY dlt3)

  printPolyLine [v1, v2, v3, v4, v5]

printTestStripes :: GCode ()
printTestStripes = section "Test Stripes" $ do
  moveToZ (fromMm 0.2)

  section "stripe 1" do
    moveTo (pos2fromMm 5 5)
    extrude (fromMmPerSec 2000) 5
    extrudeTo (pos2fromMm 215.0 5)
    extrude (fromMmPerSec 2000) (-1)

  section "stripe 2" do
    withRetract $ withZHop $ moveTo (pos2fromMm 5 10)
    extrudeTo (pos2fromMm 215.0 10)

finalPark :: GCode ()
finalPark = do
  env <- ask

  let V3 parkX parkY parkZ = toMm env.parkingPosition

  extrude (fromMmPerSec 2000) (-3)

  moveByZ (fromMm parkZ)
  moveTo (pos2fromMm parkX parkY)

homeOrResume :: GCode ()
homeOrResume = do
  env <- ask
  st <- gcodeStateGet

  if st.currentLayer == 0
    then do
      section "autoHome" $ do
        autoHome
    else do
      section "Resume" $ do
        setPositionXYZ env.parkingPosition

cleaningOpportunity :: GCode ()
cleaningOpportunity = section "Cleaning Opportunity" do
  moveTo (pos3fromMm 0 0 2)
  playTone_
  pause (fromSecs 10)

initPrinter :: GCode a -> GCode a
initPrinter inner = do
  setUnits Millimeter

  setExtruderRelative

  moveTo (pos3fromMm 0 0 0)

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
                (delta3fromMm (cos (angle * fromIntegral i) * s) (sin (angle * fromIntegral i) * s) 0)
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

getFilamentDef :: GCode () -> GCode [FilamentSection]
getFilamentDef gcode = do
  gcode
  st <- gcodeStateGet
  pure $ reverse st.filament

printFilamentDef :: [FilamentSection] -> GCode ()
printFilamentDef = undefined