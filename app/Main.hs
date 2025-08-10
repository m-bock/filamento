module Main where

import Filamento
import Linear
import Relude

-- initPrinter $
printSketch :: GCode ()
printSketch = initPrinter do
  moveTo3 (fromMm $ V3 100 100 0.2)
  extrudeToXYZ (fromMm $ V3 100 50 0.2)

  pure ()

main :: IO ()
main = do
  saveGCodeToFile
    "out/myprint.gcode"
    printSketch
    \env ->
      env
        { lineWidth = fromMm 0.4,
          layerHeight = fromMm 0.2,
          hotendTemperature = fromCelsius 205,
          bedTemperature = fromCelsius 65,
          retractLength = fromMm 1.5
        }
