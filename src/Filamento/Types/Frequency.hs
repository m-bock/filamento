module Filamento.Types.Frequency
  ( Frequency,
    fromHz,
    toHz,
  )
where

import Relude

newtype Frequency = Frequency {hz :: Double}
  deriving (Show, Eq, Generic)

fromHz :: Double -> Frequency
fromHz f = Frequency f

toHz :: Frequency -> Double
toHz (Frequency f) = f