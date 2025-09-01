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
    whileSketchZ_,
    initPrinter,
    filamentChange,
    nextLayer,
    printLayers,
    whileTrue,
    getLayerProgress,
    printLayers_,
    withColors,
    printSketchFrame,
    whileSketchZ,
    getFilamentDef,
    getZProgress,
  )
where

import Control.Monad.Writer
-- import Data.List ((!!))

-- import Filamento.Math (linspaceByStep)

import Data.List ((!!))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Filamento.Classes
import Filamento.Core
import Filamento.Math (linspaceByStep)
import Filamento.TypeOps
import Linear (V2 (..), V3 (..))
import Relude

type GCodeColorM = WriterT [(Text, GCode ())] GCode ()

withColors :: ((Text -> GCode () -> GCodeColorM) -> GCodeColorM) -> GCode ()
withColors f = do
  r <- execWriterT (f \txt gc -> tell [(txt, gc)])
  env <- ask
  let emptyMap =
        env.colors
          & fmap (\x -> (x, []))
          & toList
          & Map.fromList

      mp = foldr (\(col, gcode) accum -> Map.insertWith (++) col [gcode] accum) emptyMap r

  forM_ (zip [0 ..] $ Map.toList mp) $ \(i, (color, gcs)) -> section ("color " <> color) do
    changeColor color
    setTool i
    forM_ gcs $ \gc -> gc

whileSketchZ_ :: GCode () -> GCode ()
whileSketchZ_ inner = whileSketchZ propMax inner

whileSketchZ :: Proportion -> GCode () -> GCode ()
whileSketchZ prop inner = do
  st <- gcodeStateGet
  env <- ask
  let V3 _ _ sketchHeight = toMmF env.sketchSize
  let V3 _ _ z = toMmF st.currentPosition
  if z < (toDouble prop) * sketchHeight
    then do
      inner
      whileSketchZ prop inner
    else pure ()

whileTrue :: GCode Bool -> GCode () -> GCode ()
whileTrue pred inner = do
  b <- pred
  if b
    then do
      inner
      whileTrue pred inner
    else pure ()

getZProgress :: GCode Proportion
getZProgress = do
  st <- gcodeStateGet
  env <- ask
  let V3 _ _ sketchHeight = toMmF env.sketchSize
  let V3 _ _ z = toMmF st.currentPosition
  pure (fromMaybe propMax $ propFromDouble (z / sketchHeight))

-- TODO: This works only if firstLayerHeight equals layerHeight
getLayerProgress :: GCode OutOf
getLayerProgress = do
  st <- gcodeStateGet
  env <- ask
  let V3 _ _ sketchHeight = toMmF env.sketchSize
  let layerTotal = fromNat (round (sketchHeight / toMm env.layerHeight)) :: Total
  pure $ outOfFromCountTotal (fromNat st.currentLayer) layerTotal

printLayers :: (OutOf -> GCode ()) -> GCode ()
printLayers printLayer = section "layers" do
  env <- ask
  let V3 _ _ sketchHeight = toMmF env.sketchSize
  let countLayers = round ((sketchHeight - toMm env.firstLayerHeight) / toMm env.layerHeight)
  forM_ [0 .. countLayers - 1] $ \i -> section (show i) do
    let outOf = outOfFromCountTotal (fromNat i) (fromNat countLayers)
    nextLayer
    printLayer outOf

printLayers_ :: GCode () -> GCode ()
printLayers_ gcode = printLayers (const gcode)

printSketchFrame :: GCode ()
printSketchFrame = section "sketchFrame" do
  env <- ask
  let size2d = v3DeltaDropZ env.sketchSize
  let centerBed = add mempty (scale @Factor 0.5 env.bedSize - scale @Factor 0.5 size2d)
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
  let V3 _ _ z = toMmF st.currentPosition
  moveToZ (add (fromMm z) env.zHop)
  ret <- inner
  moveToZ (fromMm z)
  pure ret

printPolyLine :: [V3 Position] -> GCode ()
printPolyLine [] = pure ()
printPolyLine (v : vs) = do
  moveTo v
  extrudePoints vs

extrudePoints :: [V3 Position] -> GCode ()
extrudePoints vs = do
  forM_ vs $ \v -> do
    extrudeTo v

printRect2d :: V2 Position -> V2 Delta -> GCode ()
printRect2d (toMmF -> V2 x y) delta = do
  (toMmF -> V3 _ _ z) <- getCurrentPosition
  let pos = fromMmF $ V3 x y z
  printRect pos delta

printRect :: V3 Position -> V2 Delta -> GCode ()
printRect v1 (toMmF -> V2 dx dy) = section "printRect" do
  let dlt3 = fromMmF $ V3 dx dy 0
  let v2 = add v1 (justX dlt3)
  let v3 = add v2 (justY dlt3)
  let v4 = sub v3 (justX dlt3)
  let v5 = sub v4 (justY dlt3)

  printPolyLine [v1, v2, v3, v4, v5]

printTestStripes :: GCode ()
printTestStripes = section "Test Stripes" $ do
  moveToZ (fromMm 0.2)

  section "stripe 1" do
    moveTo (v2PosFromMm 5 5)
    extrude 5
    extrudeTo (v2PosFromMm 5 215.0)
    extrude (-1)

  section "stripe 2" do
    withRetract $ withZHop $ moveTo (v2PosFromMm 10 5)
    extrudeTo (v2PosFromMm 10 215.0)

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
  moveTo (v3PosFromMm 0 0 2)
  playTone_
  pause (fromSecs 10)

initPrinter :: GCode a -> GCode a
initPrinter inner = do
  section "init" $ do
    setUnits Millimeter

    setExtruderRelative

    heatup homeOrResume

  -- cleaningOpportunity

  -- printTestStripes

  ret <- inner

  section "park" $ do
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

printPolygon :: Int -> V3 Position -> Delta -> GCode ()
printPolygon n v s'
  | n < 3 = pure () -- Polygons need at least 3 sides
  | s' <= 0 = pure () -- Side length must be positive
  | otherwise = do
      let angle = 2 * pi / fromIntegral n
          s = toMm s'
          points =
            [ add @(V3 Position) @(V3 Delta)
                v
                (fromMmF $ V3 (cos (angle * fromIntegral i) * s) (sin (angle * fromIntegral i) * s) 0)
              | i <- [0 .. n - 1]
            ]
      case viaNonEmpty head points of
        Nothing -> pure ()
        Just firstPoint -> printPolyLine (points ++ [firstPoint])

filamentChange :: GCode ()
filamentChange = do
  section "Filament Change" $ do
    comment "Split below"

    heatup finalPark

    playTone_

    -- raw "M0" "Pause for filament change"

    hookUserInput "filamentChange a"

    setUnits Millimeter
    setExtruderRelative

    local (\env -> env {extrudeSpeed = fromMmPerMin 150}) $ do
      extrude (fromMm 80)

    playTone_

    hookUserInput "filamentChange b"

    -- raw "M0" "Pause for filament change"

    setUnits Millimeter
    setExtruderRelative

    -- extrude 30

    -- pause (fromSecs 2)

    playTone_

-------

-- purge TODO: implement or remove

getFilamentDef :: GCodeEnv -> GCodeState -> GCode () -> IO [FilamentSection]
getFilamentDef env state' gcode = do
  (_, finalState) <- gcodeRun gcode env state'
  pure $ finalState.filament & NE.reverse & NE.filter (\x -> x.endPos /= 0)

-- printFilamentDef TODO: implement or remove

data Dir = Vert | Horz
  deriving (Show, Eq)

purgeTower :: V2 Position -> Delta -> Int -> GCode ()
purgeTower (toMmF -> V2 x y) (toMm -> size) purgeIndex = section "purgeTower" do
  st <- gcodeStateGet
  let dir = if odd st.currentLayer then Vert else Horz

  let ticks = map toMm $ linspaceByStep (fromMm 0) (fromMm size) (fromMm 0.4) deltaFloor

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
            withRetract $ withZHop $ moveTo (v2PosFromMm x (y + tick))
            extrudeByX (fromMm size)
        Horz -> do
          section "horizontal" do
            withRetract $ withZHop $ moveTo (v2PosFromMm (x + tick) y)
            extrudeByY (fromMm size)
