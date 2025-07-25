module Marlin.DSL where

import Control.Monad.Writer
import Marlin.Core
import Relude

linearMove :: LinearMove
linearMove =
  LinearMove
    { _x = Nothing,
      _y = Nothing,
      _z = Nothing,
      _e = Nothing,
      _f = Nothing
    }

data Units = Millimeter | Inche

setUnits :: Units -> Writer [GCode] ()
setUnits u = tell $ pure $ case u of
  Millimeter -> GMillimeterUnits
  Inche -> GInchUnits

setBedTemperature :: SetBedTemperature
setBedTemperature = SetBedTemperature {_temperature = Nothing}

waitForBedTemperature :: WaitForBedTemperature
waitForBedTemperature = WaitForBedTemperature {_temperature = Nothing}

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

instance HasTargetTemperature SetBedTemperature where
  setTargetTemperature t obj = obj {_temperature = Just t}

instance HasX LinearMove where
  setX x' obj = obj {_x = Just x'}

class IsGCode a where
  run :: a -> GCode

instance IsGCode LinearMove where
  run = GLinearMove

instance IsGCode SetBedTemperature where
  run = GSetBedTemperature

render :: [GCode] -> Text
render = unlines . map (toText . toGCodeLine)