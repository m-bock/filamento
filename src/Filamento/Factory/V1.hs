module Filamento.Factory.V1 where

import Data.List ((!!))
import Filamento
import Relude

data Profile = Hill | Valley

printRectByCenter :: Position2D -> Delta2D -> GCode ()
printRectByCenter center delta = do
  let deltaHalf = scale 0.5 delta
      p1 = subDelta center deltaHalf
      p2 = addDelta p1 (justX deltaHalf)
      p3 = addDelta p2 (justY deltaHalf)
      p4 = subDelta p3 (justX deltaHalf)
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
    { overlap = fromMm 5.0,
      filamentDia = fromMm 1.75
    }

getY :: Count -> Delta -> Delta
getY c len =
  let baseLen = configDefault.overlap + len + configDefault.overlap
   in baseLen

printProfile :: Profile -> Position -> Delta -> GCode ()
printProfile profile posY len = do
  let rectCenter = addDelta (pos2FromPos 0 posY) (delta2FromDelta mempty (scale 0.5 len))
      size = delta2FromDelta configDefault.filamentDia (getY mempty len)
  printRectByCenter rectCenter size

printFilament :: [FilamentSection] -> GCode ()
printFilament secs = do
  forM_ (zip [0 ..] secs) $ \(i, sec) -> do
    let secPrev = secs !!? (i - 1)
        begin = maybe 0 (\x -> x.endPosMm) secPrev
        end = sec.endPosMm
        sdist = signedDistance begin end
    printProfile Hill begin sdist