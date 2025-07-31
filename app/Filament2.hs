{-# OPTIONS_GHC -Wno-type-defaults #-}

module Filament2 where

import Data.Text (center)
import qualified Data.Text as T
import Linear (V3 (..))
import Linear.V2 (V2 (..))
import Marlin.DSL
import Marlin.Lib
import Relude

changeEnv :: GCodeEnv -> GCodeEnv
changeEnv env =
  env

constSectionItemCount :: Int
constSectionItemCount = 8

constSections :: Int
constSections = constSectionItemCount * 10

circleCenter :: V2 Double
circleCenter = V2 120 120

circleRadius :: Double
circleRadius = 90

circleRadiusV :: V2 Double
circleRadiusV = V2 circleRadius circleRadius

constLineWidth :: Double
constLineWidth = 0.4

angle = 2 * pi / fromIntegral constSections

f :: (Double, [Double], [Int], [Int]) -> GCode ()
f = \(z, radiusOffsets, fields, safeMoves) -> do
  forM_ radiusOffsets \radiusOffset ->
    forM_ (take (1 * constSectionItemCount) [0 .. constSections - 1]) \i -> do
      let i1 = fromIntegral i
          i2 = fromIntegral (i + 1)
          angle1 = angle * i1
          angle2 = angle * i2
          vec1 = V2 (cos angle1) (sin angle1)
          vec2 = V2 (cos angle2) (sin angle2)

          nInHill = i `mod` 8

      when (nInHill `elem` fields) $ do
        do
          let p1 = circleCenter + vec1 * (pure $ circleRadius - radiusOffset * constLineWidth)
              p2 = circleCenter + vec2 * (pure $ circleRadius - radiusOffset * constLineWidth)

          when (nInHill `elem` safeMoves) do
            moveZ 2
            moveTo p1

          moveZ z
          printPolyLine [p1, p2]

printColor :: Bool -> GCode ()
printColor color = do
  forM_
    [ (0.4, [-1.5, -0.5, 0.5], [1, 2, 3, 4, 5, 6, 7], []),
      (0.8, [-2, -1, 0, 1], [2, 3, 4, 5, 6], []),
      (1.2, [-2, -1, 0, 1], [3, 4, 5], []),
      (1.6, [-1.5, -0.5, 0.5], [4], [])
    ]
    f

  forM_
    [ (0.4, [-1.5, -0.5, 0.5], [0], [0])
    -- (0.8, [-2, -1, 0, 1], [2, 3, 4, 5, 6], [0, 1, 2, 3, 4, 5, 6, 7, 8])
    -- (1.2, [-2, -1, 0, 1], [3, 4, 5], [0, 1, 2, 3, 4, 5, 6, 7, 8]),
    -- (1.6, [-1.5, -0.5, 0.5], [4], [0, 1, 2, 3, 4, 5, 6, 7, 8])
    ]
    f

sketch :: GCode ()
sketch = local changeEnv $ initPrinter $ do
  env <- ask

  -- raw "M106 S255" "Turn on fan"

  printColor True

-- printColor False

main :: IO ()
main = do
  let codeStr = toText sketch
  writeFileText "out/myprint.gcode" codeStr
  putStrLn $ T.unpack codeStr