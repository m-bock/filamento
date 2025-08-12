module PurgeTower where

import Filamento
import Filamento.IO
import Filamento.Math
import GHC.List ((!!))
import Linear
import Relude

data Dir = Vert | Horz
  deriving (Show, Eq)

purgeTower :: Position2D -> Delta -> Dir -> Int -> GCode ()
purgeTower (toMm -> V2 x y) (toMm -> size) dir purgeIndex = do
  let ticks = linspaceByStepLength 0 size 0.4 floor

  let n = 5

  let nSections = length ticks `div` n
  let nTicksPerSection = length ticks `div` nSections

  section "purgeTower" do
    forM_ [0 .. nSections - 1] \i -> section ("section " <> show i) do
      let index = i * nTicksPerSection + ((purgeIndex + i) `mod` n)
      let tick = ticks !! index
      case dir of
        Vert -> do
          section "vertical" do
            withRetract $ withZHop $ moveTo (pos2fromMm y (x + tick))
            extrudeByX (fromMm size)
        Horz -> do
          section "horizontal" do
            withRetract $ withZHop $ moveTo (pos2fromMm (x + tick) y)
            extrudeByY (fromMm size)

printSketch :: GCode ()
printSketch = do
  initPrinter do
    let pos = fromMm $ V2 100 100
        delta = fromMm 20

    forM_ [0 .. 199] \i -> do
      if i == 0
        then setFanSpeedOff
        else setFanSpeedFull

      let h = 0.2 + fromIntegral i * 0.2
      moveToZ (fromMm h)
      let dir = if odd i then Vert else Horz

      purgeTower pos delta dir 0

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
              colors = "red" :| ["yellow"]
            }
      }