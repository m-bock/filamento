{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Marlin.DSL where

import Control.Monad.Writer
import Foreign (new)
import Marlin.Core
import Relude

-------------------------------------------------------------------------------
--- GCode
-------------------------------------------------------------------------------

newtype GCode a = GCode {runGCode :: Writer [GCodeLine] a}
  deriving (Functor, Applicative, Monad)

instance ToText (GCode a) where
  toText (GCode w) =
    execWriter w
      & map gcodeLineToRaw
      & toText

-------------------------------------------------------------------------------

section :: Text -> GCode a -> GCode a
section caption gc = do
  comment caption
  ret <- gc
  newline
  pure ret

newline :: GCode ()
newline = GCode $ tell [GCodeLine {cmd = Nothing, rawExtra = "", comment = Nothing}]

comment :: Text -> GCode ()
comment c = GCode $ do
  tell [GCodeLine {cmd = Nothing, rawExtra = "", comment = Just c}]

raw :: Text -> Text -> GCode ()
raw extra comm = GCode $ do
  tell [GCodeLine {cmd = Nothing, rawExtra = extra, comment = Just comm}]

-------------------------------------------------------------------------------

class IsGCode a where
  toGCode :: a -> GCode ()

class HasX a where
  x :: Double -> a -> a

class HasY a where
  y :: Double -> a -> a

class HasZ a where
  z :: Double -> a -> a

class HasExtrude a where
  extrude :: Double -> a -> a

class HasSpeed a where
  speed :: Int -> a -> a

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
  x x' obj = obj {_x = Just x'}

instance HasY LinearMove where
  y y' obj = obj {_y = Just y'}

instance HasZ LinearMove where
  z z' obj = obj {_z = Just z'}

instance HasExtrude LinearMove where
  extrude e' obj = obj {_e = Just e'}

instance HasSpeed LinearMove where
  speed f' obj = obj {_f = Just f'}

-------------------------------------------------------------------------------

data Units = Millimeter | Inche

setUnits :: Units -> GCode ()
setUnits u = gCodeFromCmd $ case u of
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

autoHome :: AutoHome
autoHome = AutoHome

autoHome_ :: GCode ()
autoHome_ = toGCode autoHome

instance IsGCode AutoHome where
  toGCode = gCodeFromCmd . GAutoHome

-------------------------------------------------------------------------------
--- Utils
-------------------------------------------------------------------------------

gCodeFromCmd :: GCodeCmd -> GCode ()
gCodeFromCmd cmd =
  GCode
    $ tell
    $ pure
    $ GCodeLine
      { cmd = (Just cmd),
        rawExtra = "",
        comment = (Just (gcodeToComment cmd))
      }

-------------------------------------------------------------------------------
