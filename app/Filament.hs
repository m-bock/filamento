{-# OPTIONS_GHC -Wno-type-defaults #-}

module Filament where

import qualified Data.Text as T
import Linear (V3 (..))
import Linear.V2 (V2 (..))
import Marlin.DSL
import Marlin.Lib
import Relude

changeEnv :: GCodeEnv -> GCodeEnv
changeEnv env =
  env

printFullRect :: V2 Double -> V2 Double -> GCode ()
printFullRect (V2 x y) (V2 w h) = do
  let idealStripWidth = 0.2
  let stripCountX = floor (w / idealStripWidth)
  let realStripWidth = w / fromIntegral stripCountX

  -- let p1 = V2 x y
  -- let p2 = V2 (x + w) y
  -- let p3 = V2 (x + w) (y + h)
  -- let p4 = V2 x (y + h)

  -- printPolyLine [p1, p2, p3, p4, p1]

  forM_ [0 .. stripCountX - 1] \i -> do
    let xOffset = fromIntegral i * realStripWidth
    let start = V2 (x + xOffset) y
    let end = V2 (x + xOffset + realStripWidth) (y + h)
    printPolyLine [start, end]

spoolRadius = 90.0

printLayer2 :: Int -> GCode ()
printLayer2 n = do
  let (count, offset) =
        if n `elem` [1, 4]
          then (3, 0.6)
          else (4, 0.8)
  forM_ [0 .. count - 1] \i -> do
    let r = spoolRadius - offset + fromIntegral i * 0.4
    printPolygon 60 (V2 110 110) r

  pure ()

printLayer :: Int -> GCode ()
printLayer n = section ("Layer " <> T.pack (show n)) $ do
  let pct = fromIntegral n / fromIntegral countLayers
  let pctRad = pct * pi
  let spoolRadius = 90.0

  let filamentWidth = sin pctRad * 1.6
  let countSections = floor (filamentWidth / 0.5) :: Int
  let sectionWidth = filamentWidth / fromIntegral countSections

  section ("debug" <> (show (("pct", pct), ("pctRad", pctRad), ("filamentWidth", filamentWidth), ("countSections", countSections), ("sectionWidth", sectionWidth)))) do
    forM_ [0 .. countSections] \i -> do
      let offset = fromIntegral i * sectionWidth
      let offset' = filamentWidth / 2
      let r = (spoolRadius - offset') + offset

      section ("debug" <> show (("i", i), ("offset", offset), ("offset'", offset'), ("r", r))) do
        printPolygon 60 (V2 110 110) r

-- filamentChange

-- forM_ [0 .. countSections] \i -> do
--   let offset = fromIntegral i * sectionWidth
--   let offset' = filamentWidth / 2
--   let r = (spoolRadius - offset') + offset

--   section ("debug" <> show (("i", i), ("offset", offset), ("offset'", offset'), ("r", r))) do
--     printPolygonDuoColor False 60 (V2 110 110) r

-- filamentChange

height = 1.75

countLayers = 4 -- floor (height / 0.4)

sketch :: GCode ()
sketch = local changeEnv $ initPrinter $ do
  env <- ask

  -- raw "M106 S255" "Turn on fan"

  forM_ [1 .. countLayers] \i -> do
    -- playTone & setFrequency 1000 & setDuration 100 & toGCode
    nextLayer
    printLayer2 i

    -- when (i == 2) $ do
    filamentChange

printPolygonDuoColor :: Bool -> Int -> V2 Double -> Double -> GCode ()
printPolygonDuoColor color n v s
  | n < 3 = pure () -- Polygons need at least 3 sides
  | s <= 0 = pure () -- Side length must be positive
  | otherwise = do
      let angle = 2 * pi / fromIntegral n
      forM_ [0 .. n - 2] \i -> do
        let vec1 = v + V2 (s * cos (angle * fromIntegral i)) (s * sin (angle * fromIntegral i))
            vec2 = v + V2 (s * cos (angle * fromIntegral (i + 1))) (s * sin (angle * fromIntegral (i + 1)))
        if odd i && color
          then
            printPolyLine
              [ vec1,
                vec2
              ]
          else
            if even i && not color
              then
                printPolyLine
                  [ vec1,
                    vec2
                  ]
              else
                pure ()

--     points =
--       catMaybes
--         [ if (odd i && color) || (even i && not color)
--             then
--               Just (v + V2 (s * cos (angle * fromIntegral i)) (s * sin (angle * fromIntegral i)))
--             else
--               Nothing
--           | i <- [0 .. n - 1]
--         ]
-- case viaNonEmpty head points of
--   Nothing -> pure ()
--   Just firstPoint -> printPolyLine (points ++ [firstPoint])

main :: IO ()
main = do
  let codeStr = toText sketch
  writeFileText "out/myprint.gcode" codeStr
  putStrLn $ T.unpack codeStr