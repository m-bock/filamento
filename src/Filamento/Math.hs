module Filamento.Math where

import Linear.V2
import Relude

justX :: V2 Double -> V2 Double
justX (V2 x _) = V2 x 0

justY :: V2 Double -> V2 Double
justY (V2 _ y) = V2 0 y

linspace :: Double -> Double -> Int -> [Double]
linspace start end n =
  let step = (end - start) / fromIntegral (n - 1)
   in [start + fromIntegral i * step | i <- [0 .. n - 1]]

linspaceByStepLength :: Double -> Double -> Double -> (Double -> Int) -> [Double]
linspaceByStepLength start end idealStep f =
  let diff = end - start
      n = f (diff / idealStep)
   in linspace start end n

addX :: V2 Double -> Double -> V2 Double
addX (V2 x y) dx = V2 (x + dx) y

addY :: V2 Double -> Double -> V2 Double
addY (V2 x y) dy = V2 x (y + dy)

subX :: V2 Double -> Double -> V2 Double
subX (V2 x y) dx = V2 (x - dx) y

subY :: V2 Double -> Double -> V2 Double
subY (V2 x y) dy = V2 x (y - dy)