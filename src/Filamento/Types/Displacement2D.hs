module Filamento.Types.Displacement2D
  ( Displacement2D,
    justX,
    justY,
    fromMm,
    toMm,
    delta2FromMm,
    delta2ToMm,
  )
where

import Filamento.Types.Distance (Distance)
import qualified Filamento.Types.Distance as Distance
import Linear
import Linear.V (V)
import Relude

newtype Displacement2D = Displacement2D (V2 Double)

fromMm :: V2 Double -> Displacement2D
fromMm v = Displacement2D v

toMm :: Displacement2D -> V2 Double
toMm (Displacement2D v) = v

delta2FromMm :: V2 Double -> Displacement2D
delta2FromMm v = Displacement2D v

delta2ToMm :: Displacement2D -> V2 Double
delta2ToMm (Displacement2D v) = v

justX :: Displacement2D -> Displacement2D
justX (Displacement2D (V2 x _)) = Displacement2D (V2 x 0)

justY :: Displacement2D -> Displacement2D
justY (Displacement2D (V2 _ y)) = Displacement2D (V2 0 y)
