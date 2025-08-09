module Filamento.Types.Frequency
  ( Frequency,
    frequencyFromHz,
    frequencyToHz,
  )
where

import Relude

newtype Frequency = Frequency {hz :: Double}
  deriving (Show, Eq, Generic)

frequencyFromHz :: Double -> Frequency
frequencyFromHz f = Frequency f

frequencyToHz :: Frequency -> Double
frequencyToHz (Frequency f) = f