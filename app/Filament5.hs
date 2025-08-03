{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Filament5 where

import Control.Lens ((^.))
import qualified Data.Text as T
import Linear (V3 (..))
import Linear.V2 (V2 (..), _x, _y)
import Marlin.DSL
import Marlin.Lib
import Marlin.Math (addX, addY, justX, justY, subX)
import Relude
import Relude.Extra (un, wrap)

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
    lineWidth :: Double,
    countWaves :: Int,
    depthWave :: Double,
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
      lineWidth = 0.4,
      tubeCircumference,
      depthWave,
      countWaves,
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
    countWaves = 20
    spoolDiameter = 5.0
    depthWave = tubeCircumference / fromIntegral countWaves
    idealLayerHeight = 0.2
    layerCount = round (spoolDiameter / idealLayerHeight)
    realLayerHeight = spoolDiameter / fromIntegral layerCount

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

printSnake :: Coord V2D Tube Abs -> Coord V2D Tube Abs -> GCode ()
printSnake (Coord frontLeft) (Coord backRight) = do
  let size = backRight - frontLeft
      frontRight = frontLeft + justX size
      backLeft = backRight - justX size

  let step = config.lineWidth

  forM_ [0 .. 1] \i -> do
    let di = fromIntegral i
        plus = (di + 0.5) * step
        minus = -plus
        frontLeft' = frontLeft + V2 plus plus
        frontRight' = frontRight + V2 minus plus
        backRight' = backRight + V2 minus minus
        backLeft' = backLeft + V2 plus minus

    tubeMoveTo (Coord frontLeft')
    section ("Snake " <> show i) do
      section "Front" do
        tubeExtrudePoints (Coord frontLeft') (Coord frontRight')
      section "Right" do
        tubeExtrudePoints (Coord frontRight') (Coord backRight')
      section "Back" do
        tubeExtrudePoints (Coord backRight') (Coord backLeft')
      section "Left" do
        tubeExtrudePoints (Coord backLeft') (Coord frontLeft')

printWaveLayer :: Int -> Int -> GCode ()
printWaveLayer waveIndex layerIndex = do
  let spoolRadius = config.spoolDiameter / 2
      pct = fromIntegral layerIndex / fromIntegral config.layerCount
      pct' = ((pct * 2) - 1)
      depth = (acos pct' / (pi)) * config.depthWave

      frontLeft = V2 (-spoolRadius) ((fromIntegral waveIndex * config.depthWave) + (config.depthWave / 2 - depth / 2))
      backRight = V2 (spoolRadius) (((fromIntegral waveIndex) * config.depthWave) + (config.depthWave / 2 + depth / 2))

  printSnake (Coord frontLeft) (Coord backRight)

printWave :: Int -> GCode ()
printWave waveIndex = do
  env <- ask

  local (\e -> e {layerHeight = config.realLayerHeight}) do
    forM_ [0 .. config.layerCount - 1] \layerIndex -> do
      moveZ (fromIntegral layerIndex * config.realLayerHeight)
      printWaveLayer waveIndex layerIndex

printWaves :: GCode ()
printWaves = do
  forM_ [0 .. config.countWaves - 1] printWave

sketch :: GCode ()
sketch = initPrinter do
  printWaves

  --  printRect (Coord $ V2 (-5) (-5)) (Coord $ V2 5 50)

  -- forM_ [0 .. 0] \i -> do
  --   let di = fromIntegral i
  --   printSnake (Coord $ V2 (-5) (di * l)) (Coord $ V2 5 ((di + 1) * l))

  -- tubeMoveTo (Coord $ V2 0 0)
  -- tubeExtrudePoints (Coord $ V2 0 0) (Coord $ V2 0 (20 * (pi / 60)))

  -- tubeMoveTo (Coord $ V2 10 0)
  -- tubeExtrudePoints (Coord $ V2 10 0) (Coord $ V2 10 (20 * (pi / 60)))

  -- tubeMoveTo (Coord $ V2 0 0)
  -- tubeExtrudePoints (Coord $ V2 0 0) (Coord $ V2 10 0)

  pure ()

-- tubeMoveTo (Coord $ V2 (-10) 0)
-- tubeExtrudePoints (Coord $ V2 (-10) 0) (Coord $ V2 (-10) (pi))

-- tubeMoveTo (Coord $ V2 (10) 0)
-- tubeExtrudePoints (Coord $ V2 (10) 0) (Coord $ V2 (10) (pi))

-- tubeMoveTo (Coord $ V2 (10) 0)
-- tubeExtrudePoints (Coord $ V2 (10) 0) (Coord $ V2 (-10) (pi))

main :: IO ()
main = do
  ps <- readPersistentState
  let count = ps.count `mod` 4
  let mkEnv env =
        env
          { lineWidth = 0.4,
            layerHeight = 0.2,
            hotendTemperature = 200,
            bedTemperature = 65,
            transpose = V2 0 0 -- (150 - fromIntegral count * 50)
          }
  let codeStr = toText $ local mkEnv sketch
  writeFileText "out/myprint.gcode" codeStr

-- putStrLn $ T.unpack codeStr
