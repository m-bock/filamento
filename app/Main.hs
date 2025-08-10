module Main where

import Filamento
import Filamento.IO
import Linear
import qualified PurgeTower
import Relude

-- initPrinter $
printSketch :: GCode ()
printSketch = initPrinter do
  moveToXYZ (pos3FromMm $ V3 100 100 0.2)
  extrudeToXYZ (pos3FromMm $ V3 100 50 0.2)

  pure ()

main :: IO ()
main = do
  putStrLn "Starting Filamento application..."
  PurgeTower.main

-- saveGCodeToFile
--   "out/myprint.gcode"
--   printSketch
--   \env ->
--     env
--       { lineWidth = fromMm 0.4,
--         layerHeight = fromMm 0.2,
--         hotendTemperature = tempFromCelsius 205,
--         bedTemperature = tempFromCelsius 65,
--         retractLength = fromMm 1.5
--       }
