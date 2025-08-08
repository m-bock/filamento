module Filamento.Types.Frequency where

import Filamento.Conversions
import Relude

newtype Frequency = Frequency {hz :: Double}
  deriving (Show, Eq, Generic)

instance Convert' Hz Double Frequency where
  from' f = Frequency f
  to' (Frequency f) = f