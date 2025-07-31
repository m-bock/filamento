{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Filament3 where

import Data.Text (center)
import qualified Data.Text as T
import Linear (V3 (..))
import Linear.Affine (Point)
import Linear.V
import Linear.V2 (V2 (..))
import Marlin.DSL
import Marlin.Lib
import Relude

changeEnv :: GCodeEnv -> GCodeEnv
changeEnv env =
  env

data Constants = Constants
  { stepCountPerSection :: Int,
    sectionsCount :: Int,
    printedSectionsCount :: Int,
    stepCount :: Int,
    circleCenter :: V2 Double,
    circleRadius :: Double,
    lineWidth :: Double,
    stepAngle :: Double,
    sectionAngle :: Double
  }
  deriving (Show, Eq)

c :: Constants
c =
  Constants
    { stepCountPerSection,
      sectionsCount,
      printedSectionsCount = sectionsCount - 2,
      stepCount = stepCountPerSection * sectionsCount,
      circleCenter = V2 120 120,
      circleRadius = 90,
      lineWidth = 0.4,
      stepAngle = 2 * pi / fromIntegral (stepCountPerSection * sectionsCount),
      sectionAngle = 2 * pi / fromIntegral sectionsCount
    }
  where
    stepCountPerSection = 8
    sectionsCount = 20

printWhiteLayer1 :: GCode ()
printWhiteLayer1 = do
  moveZ 0.4

  forM_ [0 .. c.printedSectionsCount - 1] \i -> do
    let angle = c.sectionAngle * fromIntegral i

    let ( (_, a1, a2, a3, a4, a5, a6, a7, _, _, _, _, _),
          (_, b1, b2, b3, b4, b5, b6, b7, _, _, _, _, _),
          (_, c1, c2, c3, c4, c5, c6, c7, _, _, _, _, _)
          ) =
            getOuterPointBucket angle

    printPolyLine [a1, a2, a3, a4, a5, a6, a7]
    printPolyLine [b7, b6, b5, b4, b3, b2, b1]
    printPolyLine [c1, c2, c3, c4, c5, c6, c7]

printWhiteLayer2 :: GCode ()
printWhiteLayer2 = do
  moveZ 0.8

  forM_ [0 .. c.printedSectionsCount - 1] \i -> do
    let angle = c.sectionAngle * fromIntegral i

    let ( (_, _, a2, a3, a4, a5, a6, _, _, _, _, _, _),
          (_, _, b2, b3, b4, b5, b6, _, _, _, _, _, _),
          (_, _, c2, c3, c4, c5, c6, _, _, _, _, _, _),
          (_, _, d2, d3, d4, d5, d6, _, _, _, _, _, _)
          ) =
            getInnerPointBucket angle

    printPolyLine [a2, a3, a4, a5, a6]
    printPolyLine [b6, b5, b4, b3, b2]
    printPolyLine [c2, c3, c4, c5, c6]
    printPolyLine [d6, d5, d4, d3, d2]

getOuterPointBucket :: Double -> Tup3 (Tup13 (V2 Double))
getOuterPointBucket angle =
  gen3 \lenOffset offsetIdx ->
    gen13 \_ i ->
      mkPoint angle lenOffset offsetIdx i

getInnerPointBucket :: Double -> Tup4 (Tup13 (V2 Double))
getInnerPointBucket angle =
  gen4 \lenOffset offsetIdx ->
    gen13 \_ i ->
      mkPoint angle lenOffset offsetIdx i

mkPoint :: Double -> Int -> Int -> Int -> V2 Double
mkPoint angle lenOffset offsetIdx i =
  let v = angleToVec (angle + c.stepAngle * fromIntegral i)
      m = c.circleRadius + (offset * c.lineWidth)
      offset = (fromIntegral lenOffset * c.lineWidth) / 2 + fromIntegral offsetIdx * c.lineWidth
   in c.circleCenter + pure m * v

-- printRedLayer1 :: GCode ()
-- printRedLayer1 = do
--   moveZ 2

--   forM_ [0 .. constSectionsCount - 1] \i -> do
--     let angle = constSectionAngle * fromIntegral i
--     let (_, _, _, _, _, _, _, _, p8, p9, _, _, _, _) = f angle
--     let (offset0, offset1, offset2) = getOuterOffsets

--     moveTo (constCircleCenter + p8 * pure (constCircleRadius + offset0 * constLineWidth))
--     moveZ 0.4

--     forM_ (map (* constLineWidth) [offset0, offset1, offset2]) \offset -> do
--       printPolyLine
--         ( map
--             (\p -> constCircleCenter + p * pure (constCircleRadius + offset))
--             [p8, p9]
--         )

sketch :: GCode ()
sketch = local changeEnv $ initPrinter $ do
  env <- ask

  -- raw "M106 S255" "Turn on fan"

  printWhiteLayer1

-- printWhiteLayer2

-- changeFilament

-- playTone & setFrequency 440 & setDuration 1000 & toGCode

-- printRedLayer1

-- getPointBucketOuterp :: Double -> Map (Char, Int) (V2 Double)
-- getPointBucketOuterp angle =
--   Map.fromList
--     [ ('a', i, v)
--       | i <- [0 .. 13],
--         let v = constCircleCenter + pure m * angleToVec (angle + constStepAngle * fromIntegral i),
--         let m = constCircleRadius + (offset * constLineWidth),
--         let offset = -1.5
--     ]

main :: IO ()
main = do
  let codeStr = toText sketch
  writeFileText "out/myprint.gcode" codeStr
  putStrLn $ T.unpack codeStr

--------------------------------------------------------------------------------
--- Utils
--------------------------------------------------------------------------------

type Tup3 a = (a, a, a)

type Tup4 a = (a, a, a, a)

type Tup13 a = (a, a, a, a, a, a, a, a, a, a, a, a, a)

gen13 :: (Int -> Int -> a) -> Tup13 a
gen13 f = (g 0, g 1, g 2, g 3, g 4, g 5, g 6, g 7, g 8, g 9, g 10, g 11, g 12)
  where
    g = f 13

gen3 :: (Int -> Int -> a) -> Tup3 a
gen3 f = (g 0, g 1, g 2)
  where
    g = f 3

gen4 :: (Int -> Int -> a) -> Tup4 a
gen4 f = (g 0, g 1, g 2, g 3)
  where
    g = f 4

angleToVec :: Double -> V2 Double
angleToVec a =
  V2 (cos a) (sin a)