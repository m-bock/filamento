module Filamento.Types.Duration
  ( Duration,
    fromMs,
    toMs,
  )
where

import Relude

newtype Duration = Duration {ms :: Double}
  deriving (Show, Eq, Generic)

fromMs :: Double -> Duration
fromMs d = Duration d

toMs :: Duration -> Double
toMs (Duration d) = d