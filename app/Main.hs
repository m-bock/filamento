module Main where

import Filamento
import Filamento.IO
import Relude

printSketch :: GCode ()
printSketch = initPrinter $ do
  pure ()

main :: IO ()
main =
  saveGCodeToFile
    "out/myprint.gcode"
    printSketch
    \env ->
      env
        { lineWidth = 0.4,
          layerHeight = 0.2,
          hotendTemperature = tempFromCelsius 205,
          bedTemperature = tempFromCelsius 65,
          retractLength = distFromMm 1.5
        }
