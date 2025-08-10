module Filamento.Types.Proportion
  ( Proportion,
    proportionFromFractionClamped,
    clamp,
    proportionToFraction,
  )
where

import Relude

newtype Proportion = Proportion Double
  deriving (Show, Eq, Ord)

proportionFromFractionClamped :: Double -> Proportion
proportionFromFractionClamped f = Proportion (clamp 0 1 f)

clamp :: Double -> Double -> Double -> Double
clamp minVal maxVal x = max minVal (min maxVal x)

proportionToFraction :: Proportion -> Double
proportionToFraction (Proportion f) = f