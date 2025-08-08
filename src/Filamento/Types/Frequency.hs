module Filamento.Types.Frequency where

import Filamento.Conversions
import Relude

newtype Frequency = Frequency {hz :: Double}
  deriving (Show, Eq, Generic)

instance Convert Hz Frequency where
  from (Hz f) = Frequency f
  to (Frequency f) = Hz f