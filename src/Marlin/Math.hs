module Marlin.Math where

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