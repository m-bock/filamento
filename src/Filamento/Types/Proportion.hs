module Filamento.Types.Proportion
  ( Proportion,
    propFromFractionClamped,
    clamp,
    propToFraction,
    propMin,
    propMax,
  )
where

import Relude

newtype Proportion = Proportion Double
  deriving (Show, Eq, Ord)

propFromFractionClamped :: Double -> Proportion
propFromFractionClamped f = Proportion (clamp 0 1 f)

clamp :: Double -> Double -> Double -> Double
clamp minVal maxVal x = max minVal (min maxVal x)

propToFraction :: Proportion -> Double
propToFraction (Proportion f) = f

propMin :: Proportion
propMin = Proportion 0

propMax :: Proportion
propMax = Proportion 1