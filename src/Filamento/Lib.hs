module Filamento.Lib
  ( extrudeXY,
    extrude,
    moveXY,
    moveZ,
    nextLayer,
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
  )
where

import Filamento
import Filamento.Conversions
import Filamento.Math
import Filamento.Types.Displacement2D
import qualified Filamento.Types.Displacement2D as Disp2D
import Filamento.Types.Displacement3D (Displacement3D)
import qualified Filamento.Types.Displacement3D as Disp3D
import Filamento.Types.Distance (Distance)
import qualified Filamento.Types.Distance as Distance
import Filamento.Types.Position2D (Position2D)
import qualified Filamento.Types.Position2D as Pos2D
import Filamento.Types.Position3D (Position3D)
import qualified Filamento.Types.Position3D as Pos3D
import qualified Filamento.Types.Speed as Speed
import Linear (V2 (..), V3 (..))
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

  extrude (Speed.fromMmPerSec 2000) (-env.retractLength)

  ret <- inner

  extrude (Speed.fromMmPerSec 2000) env.retractLength

  pure ret

withZHop :: GCode a -> GCode a
withZHop inner = do
  st <- get
  env <- ask
  let V3 _ _ z = Pos3D.toMm st.currentPosition
  moveZ (z + env.zHop)
  ret <- inner
  moveZ z
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

printRect2d :: Position2D -> Displacement2D -> GCode ()
printRect2d = undefined

printRect :: Position3D -> Displacement3D -> GCode ()
printRect v1 delta = do
  let v2 = Pos3D.addDisplacement v1 (Disp3D.justX delta)
  let v3 = Pos3D.addDisplacement v2 (Disp3D.justY delta)
  let v4 = Pos3D.subtractDisplacement v3 (Disp3D.justX delta)
  let v5 = Pos3D.subtractDisplacement v4 (Disp3D.justY delta)

  printPolyLine [v1, v2, v3, v4, v5]

printTestStripes :: GCode ()
printTestStripes = section "Test Stripes" $ do
  moveZ 0.2

  -- section "Thick test stripe" do
  --   moveTo (V2 10.0 5.0)
  --   extrude 5
  --   extrudeTo (V2 215.0 5.0)
  --   extrude (-1)

  section "Thin test stripe" do
    moveToXY (fromF MM $ V2 5 5)
    extrude (Speed.fromMmPerSec 2000) 5
    extrudeXY (fromF MM $ V2 215.0 5)
    extrude (Speed.fromMmPerSec 2000) (-1)

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

  extrude (Speed.fromMmPerSec 2000) (-3)

  moveZ parkZ
  moveToXY (fromF MM $ V2 parkX parkY)

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
  moveToXYZ (Pos3D.fromMm $ V3 0 0 2)
  playTone_
  pause 10

initPrinter :: GCode a -> GCode a
initPrinter inner = do
  setUnits Millimeter

  setExtruderRelative

  moveToXYZ (Pos3D.fromMm $ V3 0 0 0)

  heatup homeOrResume

  cleaningOpportunity

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

printPolygon :: Int -> Position3D -> Distance -> GCode ()
printPolygon n v s'
  | n < 3 = pure () -- Polygons need at least 3 sides
  | s' <= 0 = pure () -- Side length must be positive
  | otherwise = do
      let angle = 2 * pi / fromIntegral n
          s = Distance.toMm s'
          points =
            [ Pos3D.addDisplacement
                v
                (Disp3D.fromMm $ V3 (cos (angle * fromIntegral i) * s) (sin (angle * fromIntegral i) * s) 0)
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

    extrude (Speed.fromMmPerSec 2000) 5

    pause 2

    local (\env -> env {extrudeSpeed = Speed.fromMmPerSec 200}) $ do
      extrude (Speed.fromMmPerSec 2000) 10

    local (\env -> env {extrudeSpeed = Speed.fromMmPerSec 800}) $ do
      extrude (Speed.fromMmPerSec 2000) 200

    local (\env -> env {extrudeSpeed = Speed.fromMmPerSec 200}) $ do
      extrude (Speed.fromMmPerSec 2000) 50
      extrude (Speed.fromMmPerSec 2000) (-1)

    playTone_

    local (\env -> env {extrudeSpeed = Speed.fromMmPerSec 200}) $ do
      extrude (Speed.fromMmPerSec 2000) (-1)
      extrude (Speed.fromMmPerSec 2000) 1

    playTone_

-------

purge :: GCode ()
purge = undefined

type FilamentDef = [(Text, Double)]

getFilamentDef :: GCode a -> FilamentDef
getFilamentDef = undefined

printFilamentDef :: FilamentDef -> GCode ()
printFilamentDef = undefined