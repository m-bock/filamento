module Filamento.Factory.V1 where

import Data.List (elemIndex, nub, (!!))
import Filamento
import Relude

data Profile = Hill | Valley

printRectByCenter :: Position2D -> Delta2D -> GCode ()
printRectByCenter center delta = do
  let deltaHalf = scale 0.5 delta
      p1 = subDelta center deltaHalf
      p2 = addDelta p1 (justX delta)
      p3 = addDelta p2 (justY delta)
      p4 = subDelta p3 (justX delta)
  moveTo p1
  extrudeTo p2
  extrudeTo p3
  extrudeTo p4
  extrudeTo p1

data Config = Config
  { overlap :: Delta,
    filamentDia :: Delta
  }

configDefault :: Config
configDefault =
  Config
    { overlap = fromMm 0, -- 5.0,
      filamentDia = fromMm 1.75
    }

getLength :: OutOf -> Profile -> Delta -> Delta
getLength c profile len =
  let baseLen = configDefault.overlap + len + configDefault.overlap
   in baseLen

getWidth :: OutOf -> Delta
getWidth _ = fromMm 100

printProfile :: Profile -> Position -> Delta -> GCode ()
printProfile profile posY len = do
  let rectCenter = addDelta (pos2FromPos 0 posY) (delta2FromDelta mempty (scale 0.5 len))

  resetLayers

  printLayers \outOf -> do
    let size = delta2FromDelta configDefault.filamentDia (getLength outOf profile len)
    printRectByCenter rectCenter size

printFilament :: [FilamentSection] -> GCode ()
printFilament secs = local
  ( \env ->
      env
        { sketchSize = delta3FromDelta 10 10 configDefault.filamentDia,
          lineWidth = fromMm 0.4,
          layerHeight = fromMm 0.2,
          hotendTemperature = fromCelsius 205,
          bedTemperature = fromCelsius 65,
          moveSpeed = fromMmPerSec 2000,
          extrudeSpeed = fromMmPerSec 2500,
          retractLength = fromMm 1.5
        }
  )
  do
    let colors = map (\x -> x.color) secs & nub
    forM_ (zip [0 ..] secs) $ \(i, sec) -> do
      let secPrev = secs !!? (i - 1)
          begin = maybe 0 (\x -> x.endPosMm) secPrev
          end = sec.endPosMm
          sdist = signedDistance begin end
          colorIndex = fromMaybe 0 (elemIndex sec.color colors)
      setTool colorIndex
      printProfile Hill begin sdist
