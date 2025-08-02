{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Filament3 where

import Data.Aeson (ToJSON, decodeStrict', encode, encodeFile)
import Data.Aeson.Decoding (decodeStrict)
import Data.Aeson.Types (FromJSON)
import Data.Text (center)
import qualified Data.Text as T
import Linear (V3 (..))
import Linear.Affine (Point)
import Linear.V
import Linear.V2 (V2 (..))
import Marlin.DSL
import Marlin.Lib
import Relude

data PrintingState = PrintingState
  {count :: Int}
  deriving (Show, Eq, Generic)

instance FromJSON PrintingState

instance ToJSON PrintingState

{-

white
000X0000
00XXX000
0XXXXX00
XXXXXXX0
012345678

red
0000XXXXXXX
00000XXXXX0
000000XXX00
0000000X000
012345678901
          ..
-}

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
      printedSectionsCount = sectionsCount,
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

    let ( (a0, a1, a2, a3, a4, a5, a6, a7, _, _, _, _),
          (b0, b1, b2, b3, b4, b5, b6, b7, _, _, _, _),
          (c0, c1, c2, c3, c4, c5, c6, c7, _, _, _, _)
          ) =
            getOuterPointBucket angle

    let line1 = [a0, a1, a2, a3, a4, a5, a6, a7]
        line2 = [b7, b6, b5, b4, b3, b2, b1, b0]
        line3 = [c0, c1, c2, c3, c4, c5, c6, c7]

    printPolyLine (line1 ++ line2 ++ line3)

printWhiteLayer2 :: GCode ()
printWhiteLayer2 = do
  moveZ 0.8

  forM_ [0 .. c.printedSectionsCount - 1] \i -> do
    let angle = c.sectionAngle * fromIntegral i

    let ( (_, a1, a2, a3, a4, a5, a6, _, _, _, _, _),
          (_, b1, b2, b3, b4, b5, b6, _, _, _, _, _),
          (_, c1, c2, c3, c4, c5, c6, _, _, _, _, _),
          (_, d1, d2, d3, d4, d5, d6, _, _, _, _, _)
          ) =
            getInnerPointBucket angle

    let line1 = [a1, a2, a3, a4, a5, a6]
        line2 = [b6, b5, b4, b3, b2, b1]
        line3 = [c1, c2, c3, c4, c5, c6]
        line4 = [d6, d5, d4, d3, d2, d1]

    printPolyLine (line1 ++ line2 ++ line3 ++ line4)

printWhiteLayer3 :: GCode ()
printWhiteLayer3 = do
  moveZ 1.2

  forM_ [0 .. c.printedSectionsCount - 1] \i -> do
    let angle = c.sectionAngle * fromIntegral i

    let ( (_, _, a2, a3, a4, a5, _, _, _, _, _, _),
          (_, _, b2, b3, b4, b5, _, _, _, _, _, _),
          (_, _, c2, c3, c4, c5, _, _, _, _, _, _),
          (_, _, d2, d3, d4, d5, _, _, _, _, _, _)
          ) =
            getInnerPointBucket angle

    let line1 = [a2, a3, a4, a5]
        line2 = [b5, b4, b3, b2]
        line3 = [c2, c3, c4, c5]
        line4 = [d5, d4, d3, d2]

    printPolyLine (line1 ++ line2 ++ line3 ++ line4)

printWhiteLayer4 :: GCode ()
printWhiteLayer4 = do
  moveZ 1.6

  forM_ [0 .. c.printedSectionsCount - 1] \i -> do
    let angle = c.sectionAngle * fromIntegral i

    let ( (_, _, _, a3, a4, _, _, _, _, _, _, _),
          (_, _, _, b3, b4, _, _, _, _, _, _, _),
          (_, _, _, c3, c4, _, _, _, _, _, _, _)
          ) =
            getOuterPointBucket angle

    let line1 = [a3, a4]
        line2 = [b4, b3]
        line3 = [c3, c4]

    printPolyLine (line1 ++ line2 ++ line3)

printRed :: GCode ()
printRed = do
  forM_ [0 .. c.printedSectionsCount - 1] \i -> do
    let angle = c.sectionAngle * fromIntegral i
    moveZ 2.0
    raw "M106 S0" "Turn off fan"
    printRedLayer1 angle
    raw "M106 S255" "Turn on fan"
    printRedLayer2 angle
    printRedLayer3 angle
    printRedLayer4 angle

printRedLayer1 :: Double -> GCode ()
printRedLayer1 angle = do
  let ( (_, _, _, _, _, _, _, a7, a8, _, _, _),
        (_, _, _, _, _, _, _, b7, b8, _, _, _),
        (_, _, _, _, _, _, _, c7, c8, _, _, _)
        ) =
          getOuterPointBucket angle

  let line1 = [a7, a8]
      line2 = [b8, b7]
      line3 = [c7, c8]

  moveTo a7
  moveZ 0.4
  printPolyLine (line1 ++ line2 ++ line3)

printRedLayer2 :: Double -> GCode ()
printRedLayer2 angle = do
  let ( (_, _, _, _, _, _, a6, a7, a8, a9, _, _),
        (_, _, _, _, _, _, b6, b7, b8, b9, _, _),
        (_, _, _, _, _, _, c6, c7, c8, c9, _, _),
        (_, _, _, _, _, _, d6, d7, d8, d9, _, _)
        ) =
          getInnerPointBucket angle

  let line1 = [a6, a7, a8, a9]
      line2 = [b9, b8, b7, b6]
      line3 = [c6, c7, c8, c9]
      line4 = [d9, d8, d7, d6]

  moveTo a6
  moveZ 0.8
  printPolyLine (line1 ++ line2 ++ line3 ++ line4)

printRedLayer3 :: Double -> GCode ()
printRedLayer3 angle = do
  let ( (_, _, _, _, _, a5, a6, a7, a8, a9, a10, _),
        (_, _, _, _, _, b5, b6, b7, b8, b9, b10, _),
        (_, _, _, _, _, c5, c6, c7, c8, c9, c10, _),
        (_, _, _, _, _, d5, d6, d7, d8, d9, d10, _)
        ) =
          getInnerPointBucket angle

  let line1 = [a5, a6, a7, a8, a9, a10]
      line2 = [b10, b9, b8, b7, b6, b5]
      line3 = [c5, c6, c7, c8, c9, c10]
      line4 = [d10, d9, d8, d7, d6, d5]

  moveTo a5
  moveZ 1.2
  printPolyLine (line1 ++ line2 ++ line3 ++ line4)

printRedLayer4 :: Double -> GCode ()
printRedLayer4 angle = do
  let ( (_, _, _, _, a4, a5, a6, a7, a8, a9, a10, a11),
        (_, _, _, _, b4, b5, b6, b7, b8, b9, b10, b11),
        (_, _, _, _, c4, c5, c6, c7, c8, c9, c10, c11)
        ) =
          getOuterPointBucket angle

  let line1 = [a4, a5, a6, a7, a8, a9, a10, a11]
      line2 = [b11, b10, b9, b8, b7, b6, b5, b4]
      line3 = [c4, c5, c6, c7, c8, c9, c10, c11]

  moveTo a4
  moveZ 1.6
  printPolyLine (line1 ++ line2 ++ line3)

printWhite :: GCode ()
printWhite = do
  raw "M106 S0" "Turn off fan"
  printWhiteLayer1
  raw "M106 S255" "Turn on fan"
  printWhiteLayer2
  printWhiteLayer3
  printWhiteLayer4

getOuterPointBucket :: Double -> Tup3 (Tup12 (V2 Double))
getOuterPointBucket angle =
  gen3 \lenOffset offsetIdx ->
    gen12 \_ i ->
      mkPoint angle lenOffset offsetIdx i

getInnerPointBucket :: Double -> Tup4 (Tup12 (V2 Double))
getInnerPointBucket angle =
  gen4 \lenOffset offsetIdx ->
    gen12 \_ i ->
      mkPoint angle lenOffset offsetIdx i

mkPoint :: Double -> Int -> Int -> Int -> V2 Double
mkPoint angle lenOffset offsetIdx i =
  let v = angleToVec (angle + c.stepAngle * fromIntegral i)
      m = (c.circleRadius - (fromIntegral lenOffset * c.lineWidth) / 2) + (fromIntegral offsetIdx * c.lineWidth)
   in c.circleCenter + pure m * v

sketch :: GCode ()
sketch = initPrinter $ do
  env <- ask

  printWhite

  filamentChange

  printRed

-- printPolygon 10 (V2 100 100) 80

-- forM_ [1 .. 4] \i -> do
--   moveZ 0.4

--   moveTo (V2 (fromIntegral i * 30) 30)
--   extrudeTo (V2 (fromIntegral i * 30 + 30) 30)

--   moveZ 10.0

-- forM_ [1 .. 4] \i -> do
--   moveZ 0.8
--   moveTo (V2 (fromIntegral i * 30) 30)
--   extrudeTo (V2 (fromIntegral i * 30 + 30) 30)

--   moveZ 10.0

-- printWhite

-- -- -- playTone & setFrequency 440 & setDuration 1000 & toGCode

-- filamentChange

-- printRed

-- raw "M106 S0" "Turn off fan"
-- moveZ 0.4
-- raw "M106 S255" "Turn on fan"
-- printSquare (V2 120 20) (V2 100 30)
-- moveZ 0.8
-- printSquare (V2 120 20) (V2 100 30)
-- moveZ 1.2
-- printSquare (V2 120 20) (V2 100 30)
-- moveZ 1.6
-- printSquare (V2 120 20) (V2 100 30)
-- moveZ 2.0
-- printSquare (V2 120 20) (V2 100 30)
-- moveZ 2.4
-- printSquare (V2 120 20) (V2 100 30)
-- moveZ 2.8
-- printSquare (V2 120 20) (V2 100 30)
-- moveZ 3.2
-- printSquare (V2 120 20) (V2 100 30)

readPrintingStateFile :: IO PrintingState
readPrintingStateFile = do
  c <- readFileBS "printing-state.json"
  v <- case decodeStrict c of
    Just x -> pure x
    Nothing -> error "Failed to decode printing-state.json"

  let v' :: PrintingState
      v' = v {count = v.count + 1}
  encodeFile "printing-state.json" v'

  pure v

main :: IO ()
main = do
  c <- readFileBS "printing-state.json"

  v :: PrintingState <- case decodeStrict c of
    Just x -> pure x
    Nothing -> error "Failed to decode printing-state.json"

  let v2 :: PrintingState
      v2 = v {count = v.count + 1}

  encodeFile "printing-state.json" v2

  let n = v.count `mod` 3

  let changeEnv env =
        env
          { bedTemperature = 65,
            -- extrudeSpeed = 1200,
            hotendTemperature = 190,
            moveSpeed = 1000,
            transpose = V2 0 0 -- (150 - fromIntegral n * 50)
          }

  let codeStr = toText $ local changeEnv sketch
  writeFileText "out/myprint.gcode" codeStr
  putStrLn $ T.unpack codeStr

--------------------------------------------------------------------------------
--- Utils
--------------------------------------------------------------------------------

type Tup3 a = (a, a, a)

type Tup4 a = (a, a, a, a)

type Tup12 a = (a, a, a, a, a, a, a, a, a, a, a, a)

gen12 :: (Int -> Int -> a) -> Tup12 a
gen12 f = (g 0, g 1, g 2, g 3, g 4, g 5, g 6, g 7, g 8, g 9, g 10, g 11)
  where
    g = f 12

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