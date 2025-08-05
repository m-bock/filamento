{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Filament5 where

import Control.Lens ((^.))
import Data.List ((!!))
import qualified Data.Text as T
import Linear (V3 (..))
import Linear.V2 (V2 (..), _x, _y)
import Marlin.DSL
import Marlin.Lib
import Marlin.Math (addX, addY, justX, justY, subX)
import Relude
import Relude.Extra (un, wrap)
import Sketch01 (parkPosition)

newtype Coord a b c = Coord a
  deriving (Show, Eq, Num)

type V2D = V2 Double

type V3D = V3 Double

type D3 = V3 Double

type X = Double

type Y = Double

type Z = Double

data Abs

data Rel

data Tube

data World

---

---

data Config = Config
  { tubeCenter :: Coord V2D World Abs,
    tubeDiameter :: Double,
    tubeRadius :: Double,
    tubeCircumference :: Double,
    countArcSteps :: Int,
    arcStep :: Double,
    idealLineWidth :: Double,
    countHills :: Int,
    countPrintedHills :: Int,
    countPrintedValleys :: Int,
    countPrintedLayers :: Int,
    depthHill :: Double,
    spoolDiameter :: Double,
    layerCount :: Int,
    idealLayerHeight :: Double,
    realLayerHeight :: Double
  }

config :: Config
config =
  Config
    { tubeCenter,
      tubeDiameter,
      tubeRadius,
      countArcSteps,
      arcStep,
      idealLineWidth = 0.4,
      tubeCircumference,
      depthHill,
      countHills,
      countPrintedHills,
      countPrintedValleys,
      countPrintedLayers,
      spoolDiameter,
      layerCount,
      idealLayerHeight,
      realLayerHeight
    }
  where
    tubeCenter = Coord (V2 120 120)
    tubeDiameter = 200
    tubeRadius = tubeDiameter / 2
    tubeCircumference = pi * tubeDiameter
    countArcSteps = 50
    arcStep = 2 * pi / fromIntegral countArcSteps
    countHills = 60
    countPrintedHills = 20 -- 20
    countPrintedValleys = countPrintedHills - 1
    spoolDiameter = 1.65
    depthHill = tubeCircumference / fromIntegral countHills
    idealLayerHeight = 0.1
    layerCount = round (spoolDiameter / idealLayerHeight)
    realLayerHeight = spoolDiameter / fromIntegral layerCount
    countPrintedLayers = layerCount

---

tubeToWorld3 :: Coord V3D Tube b -> Coord V3D World b
tubeToWorld3 (Coord (V3 x y z)) = Coord (V3 x' y' z)
  where
    Coord (V2 x' y') = tubeToWorld2 (Coord $ V2 x y)

tubeToWorld2 :: Coord V2D Tube b -> Coord V2D World b
tubeToWorld2 (Coord (V2 x y)) = Coord (V2 x' y')
  where
    Coord (V2 centerX centerY) = config.tubeCenter
    radius = un config.tubeRadius + x
    x' = centerX + radius * cos (m + rad)
    y' = centerY + radius * sin (m + rad)
    rad = y / un config.tubeRadius
    m = 3 * (pi / 2)

tubeMkLine :: Coord V2D Tube Abs -> Coord V2D Tube Abs -> [Coord V2D Tube Abs]
tubeMkLine (Coord start) (Coord end) =
  let dist@(V2 _ distY) = end - start

      countSteps = max 1 (round (abs distY / config.arcStep)) :: Int

      step = dist / pure (fromIntegral countSteps)

      mkPt i =
        let iv = pure $ fromIntegral i
            p = start + step * iv
         in Coord p
   in fmap mkPt [0 .. countSteps]

tubeExtrudePoints :: Coord V2D Tube Abs -> Coord V2D Tube Abs -> GCode ()
tubeExtrudePoints (Coord start) (Coord end) = do
  let pts = tubeMkLine (Coord start) (Coord end)
  forM_ pts tubeExtrudeTo

tubeMoveTo :: Coord (V2 Double) Tube Abs -> GCode ()
tubeMoveTo (Coord pt) = do
  let Coord worldPt = tubeToWorld2 (Coord pt)
  moveTo worldPt

tubeExtrudeTo :: Coord (V2 Double) Tube Abs -> GCode ()
tubeExtrudeTo (Coord pt) = do
  let Coord worldPt = tubeToWorld2 (Coord pt)
  extrudeTo worldPt

printRect :: Coord V2D Tube Abs -> Coord V2D Tube Abs -> GCode ()
printRect (Coord frontLeft) (Coord backRight) = do
  let size = backRight - frontLeft
      frontRight = frontLeft + justX size
      backLeft = backRight - justX size

  tubeMoveTo (Coord frontLeft)
  section "Print Rect" do
    section "Front" do
      tubeExtrudePoints (Coord frontLeft) (Coord frontRight)
    section "Right" do
      tubeExtrudePoints (Coord frontRight) (Coord backRight)
    section "Back" do
      tubeExtrudePoints (Coord backRight) (Coord backLeft)

    section "Left" do
      tubeExtrudePoints (Coord backLeft) (Coord frontLeft)

splitInterval :: Double -> Double -> (Double, Int)
splitInterval big small =
  let n = round (big / small)
      d = big / fromIntegral n
   in (d, n)

printSnake :: Coord V2D Tube Abs -> Coord V2D Tube Abs -> GCode ()
printSnake (Coord frontLeft) (Coord backRight) = section "Print Snake" $ do
  let size@(V2 w _) = backRight - frontLeft
      frontRight = frontLeft + justX size
      backLeft = backRight - justX size

  comment ("Size: " <> T.pack (show size))
  comment ("Front Left: " <> T.pack (show frontLeft))
  comment ("Front Right: " <> T.pack (show frontRight))
  comment ("Back Right: " <> T.pack (show backRight))
  comment ("Back Left: " <> T.pack (show backLeft))

  let (doubleStep, count) = splitInterval (abs w) (config.idealLineWidth * 2)

  let step = doubleStep / 2

  forM_ [0 .. count - 1] \i -> do
    let di = fromIntegral i
        plus = (di + 0.5) * step
        minus = -plus
        frontLeft' = frontLeft + V2 plus plus
        frontRight' = frontRight + V2 minus plus
        backRight' = backRight + V2 minus minus
        backLeft' = backLeft + V2 plus minus
    withRetract $ withZHop $ tubeMoveTo (Coord frontLeft')

    -- tubeMoveTo (Coord frontLeft')
    local (\e -> e {lineWidth = step}) do
      section ("Snake " <> show i) do
        section "Front" do
          tubeExtrudePoints (Coord frontLeft') (Coord frontRight')
        section "Right" do
          tubeExtrudePoints (Coord frontRight') (Coord backRight')
        section "Back" do
          tubeExtrudePoints (Coord backRight') (Coord backLeft')
        section "Left" do
          tubeExtrudePoints (Coord backLeft') (Coord frontLeft')

printFilling :: Coord V2D Tube Abs -> Coord V2D Tube Abs -> GCode ()
printFilling (Coord frontLeft) (Coord backRight) = do
  let (V2 sizeX sizeY) = backRight - frontLeft

  let count = round (sizeX / config.idealLineWidth)
  let lineWidth = sizeX / fromIntegral count

  local (\e -> e {lineWidth}) do
    forM_ [0 .. count - 1] \i -> do
      let di = fromIntegral i
          p1 = frontLeft + V2 (di * lineWidth) 0
          p2 = frontLeft + V2 (di * lineWidth) sizeY
      tubeMoveTo (Coord p1)
      tubeExtrudePoints (Coord p1) (Coord p2)

data Phase = Hill | Valley
  deriving (Show, Eq)

printPhaseLayer :: Phase -> Int -> Int -> GCode ()
printPhaseLayer phase hillIndex layerIndex = section ("Print Phase Layer " <> show phase <> " hillIndex = " <> show hillIndex <> " layerIndex = " <> show layerIndex) $ do
  let spoolRadius = config.spoolDiameter / 2
      pct = fromIntegral layerIndex / fromIntegral config.layerCount
      pct' = case phase of
        Hill -> pct
        Valley -> 1 - pct
      pct'' = (pct' * 2) - 1

      depth = (acos pct'' / pi) * config.depthHill

      extra = case phase of
        Hill -> 0
        Valley -> config.depthHill / 2

  let pctRad = 0.5 * pi + pct * pi
      radius = spoolRadius * (((abs $ cos pctRad) + 1) / 2)

  comment ("pct = " <> T.pack (show pct))
  comment ("radius = " <> T.pack (show radius))
  comment ("pctRad = " <> T.pack (show pctRad))

  let x1 = -radius
      y1 = extra + (fromIntegral hillIndex * config.depthHill) + (config.depthHill / 2 - depth / 2)

  let x2 = radius
      y2 = extra + (fromIntegral hillIndex * config.depthHill) + (config.depthHill / 2 + depth / 2)

      frontLeft = V2 x1 y1
      backRight = V2 x2 y2

  printSnake (Coord frontLeft) (Coord backRight)

printHill :: Int -> GCode ()
printHill hillIndex = section ("Print Hill " <> show hillIndex) $ do
  env <- ask

  withRetract $ moveZ 2.2
  let v = V2 0 (fromIntegral hillIndex * config.depthHill)
  withRetract $ tubeMoveTo (Coord v)

  local (\e -> e {layerHeight = config.realLayerHeight, lineWidth = config.idealLineWidth}) do
    forM_ [0 .. config.countPrintedLayers - 1] \layerIndex -> do
      if layerIndex == 0
        then raw "M106 S0" "Turn off fan"
        else raw "M106 S255" "Turn on fan"

      withRetract $ moveZ (0.1 + fromIntegral layerIndex * config.realLayerHeight)
      printPhaseLayer Hill hillIndex layerIndex

printValley :: Int -> GCode ()
printValley hillIndex = section ("Print Valley " <> show hillIndex) $ do
  env <- ask

  withRetract $ moveZ 2.2
  let v = V2 0 ((1 + fromIntegral hillIndex) * config.depthHill)
  withRetract $ tubeMoveTo (Coord v)

  local (\e -> e {layerHeight = config.realLayerHeight, lineWidth = config.idealLineWidth}) do
    forM_ [0 .. config.countPrintedLayers - 1] \layerIndex -> do
      if layerIndex == 0
        then raw "M106 S0" "Turn off fan"
        else raw "M106 S255" "Turn on fan"

      withRetract $ moveZ (0.1 + fromIntegral layerIndex * config.realLayerHeight)
      printPhaseLayer Valley hillIndex layerIndex

printFilament :: GCode ()
printFilament = do
  raw "T0" "Select tool 0"
  forM_ [0 .. config.countPrintedHills - 1] \i -> do
    printHill i

  filamentChange

  raw "T1" "Select tool 1"
  forM_ [0 .. config.countPrintedValleys - 1] printValley

  ironFinishing

ironFinishing :: GCode ()
ironFinishing = section "Iron Finishing" $ do
  raw "M106 S255" "Turn on fan"
  raw "G1 Z2.0 F1200" "Lift nozzle to avoid dragging"
  updatePos (fmap Just $ V3 0 0 2.0)

sketch :: GCode ()
sketch = initPrinter do
  printFilament
  -- printTestObj
  pure ()

printTestObj :: GCode ()
printTestObj = section "Print Test Object" $ do
  raw "M106 S0" "Turn off fan"

  forM_ [0 .. 40] \i -> do
    when (i == 1) do
      raw "M106 S255" "Turn on fan"

    printSquare (V2 10 20) (V2 50 20)
    nextLayer

isDev = False

main :: IO ()
main = do
  ps <- readPersistentState
  let count = ps.count `mod` 4
  let mkEnv env =
        env
          { lineWidth = 0.4,
            layerHeight = 0.2,
            hotendTemperature = 205,
            bedTemperature = 65,
            transpose = V2 0 if isDev then (150 - fromIntegral count * 50) else 0,
            parkingPosition = V3 0 0 30,
            moveSpeed = 2000,
            retractLength = 1.5
          }
  let codeStr = toText $ local mkEnv sketch
  writeFileText "out/myprint.gcode" codeStr
