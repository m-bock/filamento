{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Filament5 where

import qualified Data.Text as T
import Filamento
import Filamento.IO
import Filamento.Lib
import Filamento.Math (justX)
import Linear (V3 (..))
import Linear.V2 (V2 (..))
import Relude

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
    countPrintedLayers :: Int,
    depthHill :: Double,
    spoolDiameter :: Double,
    layerCount :: Int,
    idealLayerHeight :: Double,
    realLayerHeight :: Double,
    printedDepth :: Double
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
      countPrintedLayers,
      spoolDiameter,
      layerCount,
      idealLayerHeight,
      realLayerHeight,
      printedDepth = 150
    }
  where
    tubeCenter = Coord (V2 120 120)
    tubeDiameter = 200
    tubeRadius = tubeDiameter / 2
    tubeCircumference = pi * tubeDiameter
    countArcSteps = 100
    arcStep = tubeCircumference / fromIntegral countArcSteps
    spoolDiameter = 1.65
    depthHill = 50 -- tubeCircumference / fromIntegral countHills
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

    -- Use the simple fnApprox approach that works
    arcLength = y -- y coordinate represents arc length along spiral
    spiralConstant = -0.5 -- Increased magnitude for faster inward spiral
    baseRadius = config.tubeRadius -- x offset from tube radius

    -- Simple approximation: angle = arcLength / averageRadius
    -- For small spiral constants, this works very well
    averageRadius = baseRadius + spiralConstant * (arcLength / (2 * baseRadius))
    angle = arcLength / averageRadius

    -- Calculate actual spiral radius at this angle
    spiralRadius = baseRadius + spiralConstant * angle + x

    -- Convert to world coordinates
    m = 3 * (pi / 2) -- Starting angle offset
    finalAngle = m + angle

    x' = centerX + spiralRadius * cos finalAngle
    y' = centerY + spiralRadius * sin finalAngle

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
  moveXY worldPt

tubeExtrudeTo :: Coord (V2 Double) Tube Abs -> GCode ()
tubeExtrudeTo (Coord pt) = do
  let Coord worldPt = tubeToWorld2 (Coord pt)
  extrudeXY worldPt

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
    tubeMoveTo (Coord frontLeft')

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

data Phase = Hill | Valley
  deriving (Show, Eq)

printPhaseLayer :: Phase -> Int -> Int -> GCode ()
printPhaseLayer phase hillIndex layerIndex = section ("Print Phase Layer " <> show phase <> " hillIndex = " <> show hillIndex <> " layerIndex = " <> show layerIndex) $ do
  let spoolRadius = config.spoolDiameter / 2
      pct = fromIntegral layerIndex / fromIntegral config.layerCount
      pct' = case phase of
        Hill -> pct
        Valley -> 1 - pct

  let rampup = 5

  let fra = ((config.depthHill / 2) - rampup) / 2

  let depth = 2 * fra + ((1 - pct') * rampup * 2)

      extra = case phase of
        Hill -> 0
        Valley -> config.depthHill / 2

  let pctRad = 0.5 * pi + pct * pi
      radius = spoolRadius * ((abs (cos pctRad) + 1) / 2)

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

printPhase :: Phase -> Int -> GCode ()
printPhase phase hillIndex = section ("Print Phase = " <> show phase <> " hillIndex = " <> show hillIndex) $ do
  let offset = case phase of
        Hill -> 0.5
        Valley -> 1

  withRetract $ moveZ 2.2
  let v = V2 0 ((fromIntegral hillIndex + offset) * config.depthHill)
  withRetract $ tubeMoveTo (Coord v)

  withRetract $ moveZ 0.1

  local (\e -> e {layerHeight = config.realLayerHeight, lineWidth = config.idealLineWidth}) do
    forM_ [0 .. config.countPrintedLayers - 1] \layerIndex -> do
      if layerIndex == 0
        then raw "M106 S0" "Turn off fan"
        else raw "M106 S255" "Turn on fan"

      moveZ (0.1 + fromIntegral layerIndex * config.realLayerHeight)
      printPhaseLayer phase hillIndex layerIndex

printFilament :: GCode ()
printFilament = do
  raw "T0" "Select tool 0"

  let countPrintedHills = round (config.printedDepth / config.depthHill)
      countPrintedValleys = countPrintedHills - 1

  forM_ [0 .. countPrintedHills - 1] \i -> do
    printPhase Hill i

  filamentChange

  raw "T1" "Select tool 1"
  forM_ [0 .. countPrintedValleys - 1] \i -> do
    printPhase Valley i

  ironFinishing

ironFinishing :: GCode ()
ironFinishing = section "Iron Finishing" $ do
  let ironHeight = 0.1 + (fromIntegral config.countPrintedLayers - 1) * config.realLayerHeight

  raw "M106 S255" "Turn on fan"

  moveZ ironHeight
  tubeMoveTo (Coord $ V2 0 0)

  let step = config.tubeCircumference / fromIntegral config.countArcSteps

  forM_ [-(config.idealLineWidth / 2), config.idealLineWidth / 2] \offset ->
    forM_ [0 .. config.countArcSteps - 1] \i -> do
      let x = offset
          y = fromIntegral i * step

      tubeMoveTo (Coord $ V2 x y)

sketch :: GCode ()
sketch = initPrinter do
  printFilament
  -- printTestObj
  pure ()

printTestObj :: GCode ()
printTestObj = section "Print Test Object" $ do
  filamentChange

  forM_ [0 .. 40] \i -> do
    moveZ (0.1 + fromIntegral i * config.realLayerHeight)
    withRetract $ withZHop $ moveToXY (V2 100 100)
    printRect (V2 100 100) (V2 50 20)

isDev = False

main :: IO ()
main = do
  ps <- readPersistentState
  let count = ps.count `mod` 4
  let mkEnv env =
        env
          { lineWidth = 0.4,
            layerHeight = 0.2,
            hotendTemperature = Temperature 205,
            bedTemperature = Temperature 65,
            transpose = id, -- V2 0 if isDev then (150 - fromIntegral count * 50) else 0,
            parkingPosition = V3 0 0 30,
            moveSpeed = 2000,
            extrudeSpeed = 2500,
            retractLength = 1.5
          }
  let codeStr = toText $ local mkEnv sketch
  writeFileText "out/myprint.gcode" codeStr
