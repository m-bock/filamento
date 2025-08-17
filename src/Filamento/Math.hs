module Filamento.Math where

import Filamento.Classes
import Filamento.Types
import Linear.V2
import Relude

justX :: V2 Double -> V2 Double
justX (V2 x _) = V2 x 0

justY :: V2 Double -> V2 Double
justY (V2 _ y) = V2 0 y

linspace :: Position -> Position -> Count -> [Position]
linspace (toMm -> start) (toMm -> end) (toInt -> n) =
  let step = (end - start) / fromIntegral (n - 1)
   in [fromMm (start + fromIntegral i * step) | i <- [0 .. n - 1]]

linspaceByStepLength :: Position -> Position -> Delta -> (Delta -> Int) -> [Position]
linspaceByStepLength start end idealStep f =
  let dist = signedDistance start end
      n = f (dist / idealStep)
   in linspace start end (fromInt n)

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