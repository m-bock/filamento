module Filamento.Types.Continous.Factor
  ( Factor,
    mkFactor,
  )
where

import Data.Aeson.Types
import Filamento.Classes (FromDouble (..), ToDouble (..))
import GHC.Generics
import Relude

newtype Factor = Factor Double
  deriving stock (Show, Eq, Generic, Ord)
  deriving newtype (Num, Fractional, RealFrac, Real)
  deriving anyclass (ToJSON, FromJSON)
  deriving (Semigroup, Monoid) via (Sum Double)

instance FromDouble Factor where
  fromDouble x = Factor x

instance ToDouble Factor where
  toDouble (Factor x) = x

mkFactor :: Double -> Factor
mkFactor x = Factor x