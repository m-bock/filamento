module Filamento.Types.Quantities.Length (Length) where

import Filamento.Classes
import GHC.Generics
import Relude

newtype Length = Length {mm :: Double}
  deriving (Show, Eq, Generic)

instance FromToMillimeters Length where
  toMm (Length l) = l
  fromMm l = Length l