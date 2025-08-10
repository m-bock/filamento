module Filamento.Types.Frequency
  ( Frequency,
    frequencyFromHz,
    frequencyToHz,
    freqBeepLow,
    freqBeepMid,
    freqBeepHigh,
  )
where

import Relude

newtype Frequency = Frequency {hz :: Double}
  deriving (Show, Eq, Generic)

frequencyFromHz :: Double -> Frequency
frequencyFromHz f = Frequency f

frequencyToHz :: Frequency -> Double
frequencyToHz (Frequency f) = f

freqBeepLow :: Frequency
freqBeepLow = frequencyFromHz 440 -- A4, common reference pitch

freqBeepMid :: Frequency
freqBeepMid = frequencyFromHz 880 -- A5, clear but not harsh

freqBeepHigh :: Frequency
freqBeepHigh = frequencyFromHz 1320 -- E6, attention-grabbing but not painful