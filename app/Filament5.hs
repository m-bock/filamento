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
  { tubeCenter :: Coord V2D Tube Abs,
    tubeDia :: Coord Double Tube Rel,
    tubeRadius :: Coord Double Tube Rel,
    countArcSteps :: Int,
    arcStep :: Double
  }

config :: Config
config =
  Config
    { tubeCenter,
      tubeDia,
      tubeRadius = Coord (un tubeDia / 2),
      countArcSteps = countArcSteps,
      arcStep
    }
  where
    tubeCenter = Coord (V2 120 120)
    tubeDia = Coord 200
    countArcSteps = 50
    arcStep = 2 * pi / fromIntegral countArcSteps

---

tubeToWorld3 :: Coord V3D Tube b -> Coord V3D World b
tubeToWorld3 (Coord (V3 x y z)) = Coord (V3 x' y' z)
  where
    Coord (V2 x' y') = tubeToWorld2 (Coord $ V2 x y)

tubeToWorld2 :: Coord V2D Tube b -> Coord V2D World b
tubeToWorld2 (Coord (V2 x y)) = Coord (V2 x' y')
  where
    Coord (V2 centerX centerY) = config.tubeCenter
    rad = un config.tubeRadius + x
    x' = centerX + rad * cos (m + y)
    y' = centerY + rad * sin (m + y)
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

printLayer :: Double -> Double -> GCode ()
printLayer centerRad height = undefined

printWave :: Double -> GCode ()
printWave centerRad = forM_ [0 .. 10] \i -> do
  let pct = fromIntegral i / 10
  (printLayer centerRad pct)

sketch :: GCode ()
sketch = initPrinter do
  let l = 10 * (pi / 60)

  forM_ [0 .. 10] \i -> do
    let di = fromIntegral i
    printRect (Coord $ V2 (-5) (di * l)) (Coord $ V2 5 ((di + 1) * l))

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
  putStrLn $ T.unpack codeStr
