module Marlin.Core where

import Marlin.Syntax
import Relude

data LinearMove = LinearMove
  { _x :: Maybe Double,
    _y :: Maybe Double,
    _z :: Maybe Double,
    _e :: Maybe Double,
    _f :: Maybe Int
  }
  deriving (Show, Eq)

data GCode
  = --     SetMillimeterUnits

    -- | SetAbsolutePositioning
    -- | SetExtruderToAbsolute
    -- | SetBedTemperature (Maybe Int)
    -- | WaitForBedTemperature (Maybe Int)
    -- | SetHotendTemperature (Maybe Int)
    -- | WaitForHotendTemperature (Maybe Int)
    -- | AutoHome
    GLinearMove LinearMove
  deriving (-- | SetPosition (Maybe Double)
            -- | ArcMove (Maybe Double) (Maybe Double) (Maybe Double) (Maybe Double) (Maybe Double) (Maybe Int)
            -- | SetHotendTemperatureOff
            -- | SetBedTemperatureOff
            -- | DisableSteppers
            -- | Comment String
            -- | PlayTone (Maybe Int) (Maybe Int)  -- ^ PlayTone: frequency (Hz), duration (ms)
            Show, Eq)

toGCodeLine :: GCode -> GCodeLine
toGCodeLine = undefined