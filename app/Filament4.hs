{-# OPTIONS_GHC -Wno-type-defaults #-}

module Filament4 where

import qualified Data.Text as T
import Linear (V3 (..))
import Linear.V2 (V2 (..))
import Marlin.DSL
import Marlin.Lib
import Marlin.Math (justX, justY)
import Relude

spoolRadius = 90.0

height = 5 -- 1.75

width = height

lineHeight = 0.2

lineWidth = 0.4

countLayers = floor (height / lineHeight)

angleToVec :: Double -> V2 Double
angleToVec a =
  V2 (cos a) (sin a)

printLayerOld :: Bool -> Int -> GCode ()
printLayerOld color n = section ("Layer " <> T.pack (show n)) $ do
  let pct = fromIntegral n / fromIntegral countLayers
  let pctRad = pct * pi

  let filamentWidth = sin pctRad * width
  let countSections = floor (filamentWidth / lineWidth) :: Int
  let sectionWidth = filamentWidth / fromIntegral countSections

  section ("debug" <> show (("pct", pct), ("pctRad", pctRad), ("filamentWidth", filamentWidth), ("countSections", countSections), ("sectionWidth", sectionWidth))) do
    forM_ [0 .. countSections] \i -> do
      let offset = fromIntegral i * sectionWidth
      let offset' = filamentWidth / 2
      let r = (spoolRadius - offset') + offset

      section ("debug" <> show (("i", i), ("offset", offset), ("offset'", offset'), ("r", r))) do
        printPolygon' color i 60 10 40 (V2 110 110) r

printPolygon' :: Bool -> Int -> Int -> Int -> Int -> V2 Double -> Double -> GCode ()
printPolygon' color i n n' start v s
  | n < 3 = pure () -- Polygons need at least 3 sides
  | s <= 0 = pure () -- Side length must be positive
  | otherwise = do
      let angle = 2 * pi / fromIntegral n
      forM_ [0 .. n' - 1] \i -> do
        let i' = start + i
            vec1 = angleToVec (angle * fromIntegral i') * pure s + v
            vec2 = angleToVec (angle * fromIntegral (i' + 1)) * pure s + v

        when color do
          when (odd i) do
            moveTo vec1
              & withZHop
              & withRetract

            extrudeToFinal vec2

        when (not color) do
          when (even i) do
            moveTo vec1
              & withZHop
              & withRetract

            extrudeToFinal vec2

printRect :: V2 Double -> V2 Double -> GCode ()
printRect v1 s = do
  let v2 = v1 + justX s
  let v3 = v2 + justY s
  let v4 = v3 - justX s
  let v5 = v4 - justY s

  moveTo v1

  printPolyLine [v1, v2, v3, v4, v5]

printLayer :: Double -> Double -> GCode ()
printLayer centerRad height = undefined

printWave :: Double -> GCode ()
printWave centerRad = forM_ [0 .. 10] \i -> do
  let pct = fromIntegral i / 10
  (printLayer centerRad pct)

sketch :: GCode ()
sketch = do
  forM_ [0 .. 10] printWave

sketchOld :: GCode ()
sketchOld = initPrinter $ do
  env <- ask

  when False do
    raw "M106 S0" "Turn on fan off"

    nextLayer

    moveTo (V2 10 20)
      & withRetract
      & withZHop

    forM_ [0 .. 40] \i -> do
      when (i == 1) do
        raw "M106 S255" "Turn on fan"

      printSquare (V2 10 20) (V2 50 20)
      nextLayer

  do
    let firstLayer = countLayers `div` 2
    let nLayers = 1

    raw "M106 S0" "Turn on fan off"

    nextLayer

    forM_ [firstLayer .. firstLayer + nLayers - 1] \i -> do
      printLayerOld True i

    forM_ [firstLayer .. firstLayer + nLayers - 1] \i -> do
      printLayerOld False i

-- when (i == 2) $ do
--   filamentChange

main :: IO ()
main = do
  ps <- readPersistentState
  let count = ps.count `mod` 4
  let mkEnv env =
        env
          { lineWidth = lineWidth,
            layerHeight = lineHeight,
            hotendTemperature = 200,
            bedTemperature = 65,
            transpose = V2 0 (150 - fromIntegral count * 50) -- (150 - fromIntegral n * 50) }
          }
  let codeStr = toText $ local mkEnv sketch
  writeFileText "out/myprint.gcode" codeStr
  putStrLn $ T.unpack codeStr
