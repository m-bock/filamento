module Main where

import Filamento
import Filamento.IO
import Filamento.Math
import GHC.List ((!!))
import Linear
import Relude

data Dir = Vert | Horz
  deriving (Show, Eq)

purgeTower :: Position2D -> Delta -> Dir -> Int -> GCode ()
purgeTower (pos2ToMm -> V2 x y) (dltToMm -> size) dir purgeIndex = do
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
            withRetract $ withZHop $ moveToXY (pos2FromMm $ V2 y (x + tick))
            extrudeX (dltFromMm size)
        Horz -> do
          section "horizontal" do
            withRetract $ withZHop $ moveToXY (pos2FromMm $ V2 (x + tick) y)
            extrudeY (dltFromMm size)

printSketch :: GCode ()
printSketch = do
  initPrinter do
    let pos = pos2FromMm $ V2 100 100
        delta = dltFromMm 20

    forM_ [0 .. 199] \i -> do
      if i == 0
        then setFanSpeedOff
        else setFanSpeedFull

      let h = 0.2 + fromIntegral i * 0.2
      moveToZ (posFromMm h)
      let dir = if odd i then Vert else Horz

      purgeTower pos delta dir 0

{-local (\env -> env {transpose = \(V3 x y z) -> V3 x (y + 0) z})-}

main :: IO ()
main =
  run \_ ->
    ( "out/myprint.gcode",
      local mkEnv printSketch
    )
  where
    mkEnv :: GCodeEnv -> GCodeEnv
    mkEnv env =
      env
        { lineWidth = 0.4,
          layerHeight = 0.2,
          hotendTemperature = tempFromCelsius 205,
          bedTemperature = tempFromCelsius 65,
          retractLength = distFromMm 1.5
        }
