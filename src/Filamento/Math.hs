module Filamento.Math where

import Filamento.Classes
import Filamento.TypeOps
import Linear.V2
import Relude

justX :: V2 Double -> V2 Double
justX (V2 x _) = V2 x 0

justY :: V2 Double -> V2 Double
justY (V2 _ y) = V2 0 y

-- count >= 2
linspace :: Position -> Position -> Count -> [Position]
linspace _ _ n | (n < fromInt 2) = []
linspace (toMm -> start) (toMm -> end) (toInt -> n) =
  let step = (end - start) / fromIntegral (n - 1)
   in [fromMm (start + fromIntegral i * step) | i <- [0 .. n - 1]]

linspaceByStep :: Position -> Position -> Delta -> (Delta -> Count) -> [Position]
-- linspaceByStep start end _ _ | start == end = map (const start) [0 ..]
linspaceByStep _ _ d _ | d <= 0 = []
linspaceByStep start end idealStep f =
  let dist = getDelta start end
      n = f (abs dist / idealStep)
   in linspace start end (countInc n)

addX :: V2 Double -> Double -> V2 Double
addX (V2 x y) dx = V2 (x + dx) y

addY :: V2 Double -> Double -> V2 Double
addY (V2 x y) dy = V2 x (y + dy)

subX :: V2 Double -> Double -> V2 Double
subX (V2 x y) dx = V2 (x - dx) y

subY :: V2 Double -> Double -> V2 Double
subY (V2 x y) dy = V2 x (y - dy)

i2d :: (Integral a, Num b) => a -> b
i2d = fromIntegral

linspace2 :: V2 Position -> V2 Position -> Count -> [V2 Position]
linspace2 pos1 pos2 count =
  let V2 x1 y1 = pos1
      V2 x2 y2 = pos2
      xs = linspace x1 x2 count
      ys = linspace y1 y2 count
   in zipWith V2 xs ys

linspace2ByStep :: V2 Position -> V2 Position -> Delta -> (Delta -> Count) -> [V2 Position]
linspace2ByStep _ _ d _ | d <= 0 = []
linspace2ByStep pos1 pos2 idealStep f =
  let n = f (getDistance pos1 pos2 / idealStep)
   in linspace2 pos1 pos2 (countInc n)

itemsWithNext :: [a] -> [(a, a)]
itemsWithNext xs =
  case viaNonEmpty tail xs of
    Just xsTail -> zip xs xsTail
    Nothing -> []
