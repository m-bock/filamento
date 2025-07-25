module Marlin.DSL where

import Control.Monad.Writer
import Marlin.Core
import Relude

-------------------------------------------------------------------------------
--- GCode
-------------------------------------------------------------------------------

newtype GCode = GCode {runGCode :: Writer [GCodeCmd] ()}

instance ToText GCode where
  toText (GCode w) =
    execWriter w
      & map (toText . gcodeToRaw)
      & unlines

-------------------------------------------------------------------------------

class IsGCode a where
  toGCode :: a -> GCode

class HasX a where
  setX :: Double -> a -> a

class HasY a where
  setY :: Double -> a -> a

class HasZ a where
  setZ :: Double -> a -> a

class HasE a where
  setE :: Double -> a -> a

class HasF a where
  setF :: Int -> a -> a

class HasTargetTemperature a where
  setTargetTemperature :: Int -> a -> a

-------------------------------------------------------------------------------

linearMove :: LinearMove
linearMove =
  LinearMove
    { _x = Nothing,
      _y = Nothing,
      _z = Nothing,
      _e = Nothing,
      _f = Nothing
    }

instance IsGCode LinearMove where
  toGCode = gCodeFromCmd . GLinearMove

instance HasX LinearMove where
  setX x' obj = obj {_x = Just x'}

instance HasY LinearMove where
  setY y' obj = obj {_y = Just y'}

-------------------------------------------------------------------------------

data Units = Millimeter | Inche

setUnits :: Units -> GCode
setUnits u = GCode $ tell $ pure $ case u of
  Millimeter -> GMillimeterUnits
  Inche -> GInchUnits

-------------------------------------------------------------------------------

setBedTemperature :: SetBedTemperature
setBedTemperature = SetBedTemperature {_temperature = Nothing}

instance IsGCode SetBedTemperature where
  toGCode = gCodeFromCmd . MSetBedTemperature

instance HasTargetTemperature SetBedTemperature where
  setTargetTemperature t obj = obj {_temperature = Just t}

-------------------------------------------------------------------------------

waitForBedTemperature :: WaitForBedTemperature
waitForBedTemperature = WaitForBedTemperature {_temperature = Nothing}

instance IsGCode WaitForBedTemperature where
  toGCode = gCodeFromCmd . MWaitForBedTemperature

instance HasTargetTemperature WaitForBedTemperature where
  setTargetTemperature t obj = obj {_temperature = Just t}

-------------------------------------------------------------------------------

setHotendTemperature :: SetHotendTemperature
setHotendTemperature = SetHotendTemperature {_temperature = Nothing}

instance IsGCode SetHotendTemperature where
  toGCode = gCodeFromCmd . MSSetHotendTemperature

instance HasTargetTemperature SetHotendTemperature where
  setTargetTemperature t obj = obj {_temperature = Just t}

-------------------------------------------------------------------------------

waitForHotendTemperature :: WaitForHotendTemperature
waitForHotendTemperature = WaitForHotendTemperature {_temperature = Nothing}

instance IsGCode WaitForHotendTemperature where
  toGCode = gCodeFromCmd . MWaitForHotendTemperature

instance HasTargetTemperature WaitForHotendTemperature where
  setTargetTemperature t obj = obj {_temperature = Just t}

-------------------------------------------------------------------------------
--- Utils
-------------------------------------------------------------------------------

gCodeFromCmd :: GCodeCmd -> GCode
gCodeFromCmd = GCode . tell . pure

-------------------------------------------------------------------------------
