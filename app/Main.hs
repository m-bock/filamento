module Main where

-- import Data.IntMap.Lazy (restrictKeys)
-- import Filament5 (Config (layerCount))
import Filamento
import Filamento.Factory.V1
import Filamento.Math
import Linear
import Relude

printStripesAlongX :: Square2D -> Count -> [Line2D]
printStripesAlongX square count = do
  let V2 x1 y1 = square2GetMinCorner square
      V2 x2 y2 = square2GetMaxCorner square

      ys = linspace y1 y2 count

  map (\y -> line2FromPoints (V2 x1 y) (V2 x2 y)) ys

printStripesAlongY :: Square2D -> Count -> [Line2D]
printStripesAlongY square count = do
  let V2 x1 y1 = square2GetMinCorner square
      V2 x2 y2 = square2GetMaxCorner square

      xs = linspace x1 x2 count

  map (\x -> line2FromPoints (V2 x y1) (V2 x y2)) xs

printPurgeTower :: Square2D -> Count -> GCode ()
printPurgeTower square count = do
  st <- gcodeStateGet
  let linesToPrint = if odd st.currentLayer then printStripesAlongX square count else printStripesAlongY square count

  forM_ linesToPrint $ \line -> do
    let p1 = line2GetStart line
        p2 = line2GetEnd line
    moveTo p1
    extrudeTo p2

data Color = Red | Yellow
  deriving (Show, Eq)

printSketch :: GCode ()
printSketch = withSketchTranspose do
  resetLayers
  printLayers_ do
    let rect = rect2FromCenterSize (v2PosFromMm 50 50) (fromMm $ V2 50 30)
        (p1, p2, p3, p4) = rect2GetPoints rect
    withColors
      \color -> do
        color Red do
          printPurgeTower (square2FromCenterSize (v2PosFromMm 20 120) (fromMm 20)) (fromInt 20)

        color Yellow do
          printPurgeTower (square2FromCenterSize (v2PosFromMm 80 120) (fromMm 20)) (fromInt 20)

        color Red do
          moveTo p1
          extrudeTo p2

        color Yellow do
          moveTo p2
          extrudeTo p3

        color Red do
          moveTo p3
          extrudeTo p4

        color Yellow do
          moveTo p4
          extrudeTo p1

printAll :: GCode ()
printAll = initPrinter do
  -- printSketchFrame

  -- moveToZ (fromMm 0.2)
  -- testCode

  env <- ask
  st <- gcodeStateGet
  let ret = getFilamentDef env st printSketch

  -- comment (show ret)

  filamentChange

  resetLayers
  printFilament
    (\cfg -> cfg {disableSpiral = True})
    (take 10 (toList ret))

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
              colors = fmap show $ Red :| [Yellow],
              sketchSize = fromMm $ V3 100 100 10,
              parkingPosition = v3PosFromMm 0 0 20
            }
      }
