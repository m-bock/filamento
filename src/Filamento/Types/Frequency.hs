module Filamento.Types.Frequency
  ( Frequency,
    freqFromHz,
    freqToHz,
    freqBeepLow,
    freqBeepMid,
    freqBeepHigh,
  )
where

import Relude

newtype Frequency = Frequency {hz :: Double}
  deriving (Show, Eq, Generic)

freqFromHz :: Double -> Frequency
freqFromHz f = Frequency f

freqToHz :: Frequency -> Double
freqToHz (Frequency f) = f

freqBeepLow :: Frequency
freqBeepLow = freqFromHz 440 -- A4, common reference pitch

freqBeepMid :: Frequency
freqBeepMid = freqFromHz 880 -- A5, clear but not harsh

freqBeepHigh :: Frequency
freqBeepHigh = freqFromHz 1320 -- E6, attention-grabbing but not painful