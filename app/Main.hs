module Main where

import Data.IntMap.Lazy (restrictKeys)
import Filament5 (Config (layerCount))
import Filamento
import Filamento.Factory.V1
import Filamento.Math
import Linear
import Relude

printStripesAlongX :: Square2D -> Count -> [Line2D]
printStripesAlongX square count = do
  let V2 x1 y1 = pos2ToVec $ square2GetMinCorner square
      V2 x2 y2 = pos2ToVec $ square2GetMaxCorner square

      ys = linspace y1 y2 count

  map (\y -> line2FromPoints (pos2FromPos x1 y) (pos2FromPos x2 y)) ys

printStripesAlongY :: Square2D -> Count -> [Line2D]
printStripesAlongY square count = do
  let V2 x1 y1 = pos2ToVec $ square2GetMinCorner square
      V2 x2 y2 = pos2ToVec $ square2GetMaxCorner square

      xs = linspace x1 x2 count

  map (\x -> line2FromPoints (pos2FromPos x y1) (pos2FromPos x y2)) xs

printPurgeTower :: Square2D -> Count -> GCode ()
printPurgeTower square count = do
  st <- gcodeStateGet
  let lines = if odd st.currentLayer then printStripesAlongX square count else printStripesAlongY square count

  forM_ lines $ \line -> do
    let p1 = line2GetStart line
        p2 = line2GetEnd line
    moveTo p1
    extrudeTo p2

printSketch :: GCode ()
printSketch = withSketchTranspose do
  resetLayers
  printLayers_ do
    withColors
      \color -> do
        color "red" do
          printPurgeTower (square2FromCenterSize (pos2fromMm 20 120) (fromMm 20)) (fromInt 10)

        color "yellow" do
          printPurgeTower (square2FromCenterSize (pos2fromMm 80 120) (fromMm 20)) (fromInt 10)

printAll :: GCode ()
printAll = initPrinter do
  printSketchFrame

  env <- ask
  st <- gcodeStateGet
  let ret = getFilamentDef env st printSketch

  comment (show ret)

  filamentChange

  resetLayers
  printFilament (toList ret)

  filamentChange

  printSketch

main :: IO ()
main = do
  generateGcode
    OutputConfig
      { gcodeFile = "out/myprint.gcode",
        reportFile = "out/myprint-report.json",
        gcode = printAll,
        env =
          gcodeEnvDefault
            { lineWidth = fromMm 0.6,
              layerHeight = fromMm 0.3,
              hotendTemperature = fromCelsius 205,
              bedTemperature = fromCelsius 65,
              retractLength = fromMm 1.5,
              colors = "red" :| ["yellow"],
              sketchSize = fromMm3 100 100 20,
              parkingPosition = pos3fromMm 0 0 20
            }
      }
