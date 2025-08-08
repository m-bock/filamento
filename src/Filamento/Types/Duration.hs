module Filamento.Types.Duration where

import Filamento.Conversions
import Relude

newtype Duration = Duration {ms :: Double}
  deriving (Show, Eq, Generic)

instance Convert' Ms Double Duration where
  from' d = Duration d
  to' (Duration ms) = ms
