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
    withColors,
    printSketchFrame,
    getFilamentDef,
  )
where

import Control.Monad.Writer
-- import Data.List ((!!))

-- import Filamento.Math (linspaceByStepLength)

import Data.List ((!!))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Filamento.Classes
import Filamento.Core
import Filamento.Math (linspaceByStepLength)
import Filamento.TypeOps
import Linear (V2 (..), V3 (..))
import Relude

type GCodeColorM col = WriterT [(col, GCode ())] GCode ()

withColors :: (Show col) => ((col -> GCode () -> GCodeColorM col) -> GCodeColorM col) -> GCode ()
withColors f = do
  r <- execWriterT (f \txt gc -> tell [(txt, gc)])
  env <- ask
  let emptyMap =
        env.colors
          & fmap (\x -> (x, []))
          & toList
          & Map.fromList

      mp = foldr (\(col, gcode) accum -> Map.insertWith (++) (show col) [gcode] accum) emptyMap r

  forM_ (zip [0 ..] $ Map.toList mp) $ \(i, (color, gcs)) -> section ("color " <> color) do
    changeColor color
    setTool i
    forM_ gcs $ \gc -> gc

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
  let size2d = v2DeltaFrom3 env.sketchSize
  let centerBed = addDelta mempty (scale 0.5 env.bedSize - scale 0.5 size2d)
  printRect2d centerBed size2d

withRetract :: GCode a -> GCode a
withRetract inner = section "retract" do
  env <- ask

  extrude (-env.retractLength)

  ret <- inner

  extrude env.retractLength

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

printRect2d :: Position2D -> V2 Delta -> GCode ()
printRect2d (toMm -> V2 x y) delta = do
  (toMm -> V3 _ _ z) <- getCurrentPosition
  let pos = fromMm $ V3 x y z
  printRect pos delta

printRect :: Position3D -> V2 Delta -> GCode ()
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
    extrude 5
    extrudeTo (pos2fromMm 215.0 5)
    extrude (-1)

  section "stripe 2" do
    withRetract $ withZHop $ moveTo (pos2fromMm 5 10)
    extrudeTo (pos2fromMm 215.0 10)

finalPark :: GCode ()
finalPark = do
  env <- ask

  -- extrude (-3)

  moveTo env.parkingPosition

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

  -- cleaningOpportunity

  -- printTestStripes

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

printPolygon :: Int -> Position3D -> Delta -> GCode ()
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

    setUnits Millimeter

    setExtruderRelative

    -- extrude 30

    -- pause (fromSecs 2)

    playTone_

-------

-- purge TODO: implement or remove

getFilamentDef :: GCodeEnv -> GCodeState -> GCode () -> [FilamentSection]
getFilamentDef env state' gcode =
  let (_, finalState, _) = gcodeRun gcode env state'
   in finalState.filament & NE.reverse & NE.filter (\x -> x.endPosMm /= 0)

-- printFilamentDef TODO: implement or remove

data Dir = Vert | Horz
  deriving (Show, Eq)

purgeTower :: Position2D -> Delta -> Int -> GCode ()
purgeTower (toMm -> V2 x y) (toMm -> size) purgeIndex = section "purgeTower" do
  st <- gcodeStateGet
  let dir = if odd st.currentLayer then Vert else Horz

  let ticks = map toMm $ linspaceByStepLength (fromMm 0) (fromMm size) (fromMm 0.4) floor

  let n = 5

  let nSections = length ticks `div` n
  let nTicksPerSection = length ticks `div` nSections

  section "purgeTower" do
    forM_ [0 .. nSections - 1] \i -> section ("section " <> show i) do
      let index = i * nTicksPerSection + ((purgeIndex + i) `mod` n)
      let tick = ticks !! index
      case dir of
        Vert -> do
          section "vertical" do
            withRetract $ withZHop $ moveTo (pos2fromMm x (y + tick))
            extrudeByX (fromMm size)
        Horz -> do
          section "horizontal" do
            withRetract $ withZHop $ moveTo (pos2fromMm (x + tick) y)
            extrudeByY (fromMm size)
