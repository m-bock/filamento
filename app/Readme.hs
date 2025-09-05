module Readme where

import Filamento
import Relude

sketch :: GCode ()
sketch = initPrinter do
  comment "This is a sample sketch"

  printTestStripes

  moveToZ (fromMm 0.2)

  forM_ [1 .. 20] \i -> do
    comment $ "Layer " <> show i
    nextLayer

    let p1 = v2PosFromMm (50, 60)
        p2 = v2PosFromMm (75, 25)
        p3 = v2PosFromMm (80, 80)

    moveTo p1
    extrudeTo p2
    extrudeTo p3
    extrudeTo p1

  hookEmitGCode "final"

main :: IO ()
main = do
  hooks <- mkHookFileAppender "out/sample.gcode"

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