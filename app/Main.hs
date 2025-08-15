module Main where

import Data.IntMap.Lazy (restrictKeys)
import Filamento
import Filamento.Factory.V1
import Relude

p :: GCode ()
p = withSketchTranspose do
  resetLayers
  printLayers_ do
    withColors
      \color -> do
        forM_ [0 .. 3] $ \x -> do
          forM_ [(-1) .. 2] $ \y -> do
            let d = 30
            let p1 = pos2fromMm (x * d) (y * d)
                p2 = pos2fromMm (x * d + 10) (y * d)
                p3 = pos2fromMm (x * d + 15) (y * d + 15)
                p4 = pos2fromMm (x * d) (y * d + 20)

            color "red" do
              withRetract $ withZHop $ moveTo p1
              extrudeTo p2

            color "yellow" do
              withRetract $ withZHop $ moveTo p2
              extrudeTo p3

            color "red" do
              withRetract $ withZHop $ moveTo p3
              extrudeTo p4

            color "yellow" do
              withRetract $ withZHop $ moveTo p4
              extrudeTo p1

ptsFromGrid :: [String] -> [Position2D]
ptsFromGrid grid = undefined

ptsFromStr :: String -> [Position2D]
ptsFromStr str =
  let ps = indicesOf 'P'
   in undefined

indicesOf :: Char -> String -> [Int]
indicesOf c = go 0
  where
    go _ [] = []
    go i (x : xs)
      | x == c = i : go (i + 1) xs
      | otherwise = go (i + 1) xs

printSketch :: GCode ()
printSketch = initPrinter do
  env <- ask
  st <- gcodeStateGet
  let ret = getFilamentDef env st p

  comment (show ret)

  filamentChange

  resetLayers
  printFilament (takeWhile (\x -> x.endPosMm < 200) (toList ret))

  filamentChange

  p

main :: IO ()
main = do
  generateGcode
    OutputConfig
      { gcodeFile = "out/myprint.gcode",
        reportFile = "out/myprint-report.json",
        gcode = printSketch,
        env =
          gcodeEnvDefault
            { lineWidth = fromMm 0.6,
              layerHeight = fromMm 0.3,
              hotendTemperature = fromCelsius 205,
              bedTemperature = fromCelsius 65,
              retractLength = fromMm 1.5,
              colors = "red" :| ["yellow"],
              sketchSize = fromMm3 100 100 5,
              parkingPosition = pos3fromMm 0 0 50
            }
      }
