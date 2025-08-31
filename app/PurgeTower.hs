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

purgeTower :: V2 Position -> Delta -> Int -> GCode ()
purgeTower (toMm2 -> V2 x y) (toMm -> size) purgeIndex = do
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
            withRetract $ withZHop $ moveTo (v2PosFromMm y (x + tick))
            extrudeByX (fromMm size)
        Horz -> do
          section "horizontal" do
            withRetract $ withZHop $ moveTo (v2PosFromMm (x + tick) y)
            extrudeByY (fromMm size)

printSketch :: GCode ()
printSketch = do
  initPrinter do
    let pos = fromMm2 $ V2 100 100
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
main = pure ()