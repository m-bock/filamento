module PurgeTower where

import Filamento
-- import Filamento.Classes
-- import Filamento.IO
import Filamento.Math
import GHC.List ((!!))
import Linear
import Relude

data Dir = Vert | Horz
  deriving (Show, Eq)

purgeTower :: Position2D -> Delta -> Int -> GCode ()
purgeTower (toMm -> V2 x y) (toMm -> size) purgeIndex = do
  st <- gcodeStateGet
  let dir = if odd st.currentLayer then Vert else Horz

  let ticks = map toMm $ linspaceByStep (fromMm 0) (fromMm size) (fromMm 0.4) deltaFloor

  let n = 5

  let nSections = length ticks `div` n
  let nTicksPerSection = length ticks `div` nSections

  section "purgeTower" do
    forM_ [0 .. nSections - 1] \i -> section ("section " <> show i) do
      let index = i * nTicksPerSection + ((purgeIndex + i) `mod` n)
      let safeIndex = max 0 (min index (length ticks - 1))
      let tick = ticks !! safeIndex
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

    forM_ [0 .. (199 :: Int)] \i -> do
      if i == 0
        then setFanSpeedOff
        else setFanSpeedFull

      let h = 0.2 + fromIntegral i * 0.2
      moveToZ (fromMm h)
      let _dir = if odd i then Vert else Horz

      purgeTower pos delta 0

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