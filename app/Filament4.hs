{-# OPTIONS_GHC -Wno-type-defaults #-}

module Filament4 where

import qualified Data.Text as T
import Linear (V3 (..))
import Linear.V2 (V2 (..))
import Marlin.DSL
import Marlin.Lib
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

printLayer :: Bool -> Int -> GCode ()
printLayer color n = section ("Layer " <> T.pack (show n)) $ do
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

sketch :: GCode ()
sketch = initPrinter $ do
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
      printLayer True i

    forM_ [firstLayer .. firstLayer + nLayers - 1] \i -> do
      printLayer False i

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
