module Main where

-- import Data.IntMap.Lazy (restrictKeys)
-- import Filament5 (Config (layerCount))
import Filamento
import Filamento.Filament
import Filamento.Math
import GHC.Conc
import Graphics.Gnuplot.Simple
import Linear
import Relude

printStripesAlongX :: Square2D -> Count -> [Line2D]
printStripesAlongX square count = do
  let V2 x1 y1 = square2GetMinCorner square
      V2 x2 y2 = square2GetMaxCorner square

      ys = linspace y1 y2 count

  map (\y -> line2FromPoints (V2 x1 y) (V2 x2 y)) ys

printStripesAlongY :: Position -> Rect2D -> Count -> [Line2D]
printStripesAlongY z rect count = do
  let V2 x1 y1 = rect2GetMinCorner rect
      V2 x2 y2 = rect2GetMaxCorner rect

      xs = linspace x1 x2 count

      shift = z * 8

  map (\x -> line2FromPoints (V2 x (y1 + shift)) (V2 x (y2 + shift))) xs

printPurgeTower :: Rect2D -> Count -> GCode ()
printPurgeTower rect count = do
  st <- gcodeStateGet
  let V3 _ _ curZ = st.currentPosition
  let linesToPrint = printStripesAlongY curZ rect count

  forM_ (zip [0 ..] linesToPrint) $ \(i, line) -> do
    let (p1, p2) =
          if odd i
            then
              (line2GetStart line, line2GetEnd line)
            else
              (line2GetEnd line, line2GetStart line)

    if i == 0
      then do
        withRetract $ withZHop $ moveTo p1
      else do
        moveTo p1
    extrudeTo p2

data Color = Red | Yellow
  deriving (Show, Eq)

printSketch :: GCode ()
printSketch = withSketchTranspose do
  resetLayers
  printLayers_ do
    st <- gcodeStateGet
    if st.currentLayer == 1
      then do
        setFanOff
      else do
        setFanSpeedFull

    let rect = rect2FromCenterSize (v2PosFromMm 50 50) (fromMm $ V2 50 30)
        (p1, p2, p3, p4) = rect2GetPoints rect
    withColors
      \color -> do
        color Red do
          printPurgeTower (rect2FromCenterSize (v2PosFromMm (-20) (-55)) (fromMm $ V2 50 15)) (fromInt 80)

        color Yellow do
          printPurgeTower (rect2FromCenterSize (v2PosFromMm 32 (-55)) (fromMm $ V2 50 15)) (fromInt 80)

        color Red do
          withRetract $ withZHop $ moveTo p1
          extrudeTo p2

        color Yellow do
          withRetract $ withZHop $ moveTo p2
          extrudeTo p3

        color Red do
          withRetract $ withZHop $ moveTo p3
          extrudeTo p4

        color Yellow do
          withRetract $ withZHop $ moveTo p4
          extrudeTo p1

printAll :: GCode ()
printAll = initPrinter do
  -- printSketchFrame

  -- moveToZ (fromMm 0.2)
  -- testCode

  env <- ask
  st <- gcodeStateGet
  let ret = getFilamentDef env st printSketch

  -- filamentChange

  resetLayers
  printFilament
    (\cfg -> cfg {disableSpiral = False})
    (toList ret)

  filamentChange

  printSketch

mainGen :: IO ()
mainGen =
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
              sketchSize = fromMm $ V3 100 100 2,
              parkingPosition = v3PosFromMm 0 0 20
            }
      }

mainPlot :: IO ()
mainPlot = do
  pure ()

-- let out = toMm <$> getLayerHeights
-- putStrLn $ show out
-- plotList [] out
-- threadDelay 5000000 -- wait 5s

main :: IO ()
main = mainGen