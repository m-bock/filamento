module SamplePyramid where

import Filamento
import Linear (V2 (..))
import Relude

sketch :: GCode ()
sketch = initPrinter do
  comment "This is a sample sketch"

  printTestStripes

  moveToZ (fromMm 0.2)

  let countLayers = 120

  forM_ [1 .. countLayers] \i -> do
    comment $ "Layer " <> show i
    nextLayer

    let len = lengthByMm $ project (Range 1 countLayers) (Range 50 10) i

        rectX = posFromMm 100
        rectY = posFromMm 100
        rect = rect2From (RectCenter (V2 rectX rectY), RectSize (V2 len len))

        (RectCorners p1 p2 p3 p4) = rect2To rect

    moveTo p1
    extrudeTo p2
    extrudeTo p3
    extrudeTo p4
    extrudeTo p1

  hookEmitGCode "final"

main :: IO ()
main = do
  hooks <- mkHookFileAppender "out/sample-pyramid.gcode"

  gcodeLaunch
    sketch
    ( \env ->
        env
          { hookEmitGCode = hooks,
            lineWidth = fromMm 0.6,
            layerHeight = fromMm 0.3,
            firstLayerHeight = fromMm 0.3,
            hotendTemperature = fromCelsius 205,
            bedTemperature = fromCelsius 65
          }
    )