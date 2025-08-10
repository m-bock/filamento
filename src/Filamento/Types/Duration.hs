module Filamento.Types.Duration
  ( Duration,
    durFromMs,
    durToMs,
  )
where

import Relude

newtype Duration = Duration {ms :: Double}
  deriving (Show, Eq, Generic)

durFromMs :: Double -> Duration
durFromMs d = Duration d

durToMs :: Duration -> Double
durToMs (Duration d) = d