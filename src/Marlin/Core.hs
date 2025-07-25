{-# LANGUAGE TupleSections #-}

module Marlin.Core where

-- See: https://marlinfw.org/meta/gcode/

import qualified Data.Map.Strict as Map
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

data SetBedTemperature = SetBedTemperature
  { _temperature :: Maybe Int
  }
  deriving (Show, Eq)

data WaitForBedTemperature = WaitForBedTemperature
  { _temperature :: Maybe Int
  }
  deriving (Show, Eq)

data SetHotendTemperature = SetHotendTemperature
  { _temperature :: Maybe Int
  }
  deriving (Show, Eq)

data WaitForHotendTemperature = WaitForHotendTemperature
  { _temperature :: Maybe Int
  }
  deriving (Show, Eq)

data AutoHome = AutoHome
  deriving (Show, Eq)

data GCodeCmd
  = GMillimeterUnits
  | GInchUnits
  | GLinearMove LinearMove
  | GAutoHome AutoHome
  | MSetBedTemperature SetBedTemperature
  | MWaitForBedTemperature WaitForBedTemperature
  | MSSetHotendTemperature SetHotendTemperature
  | MWaitForHotendTemperature WaitForHotendTemperature
  deriving (Show, Eq)

data GCodeLine = GCodeLine
  { cmd :: Maybe GCodeCmd,
    rawExtra :: Text,
    comment :: Maybe Text
  }
  deriving (Show, Eq)

gcodeLineToRaw :: GCodeLine -> RawGCodeLine
gcodeLineToRaw (GCodeLine Nothing extra Nothing) = RawGCodeLine {cmd = Nothing, rawExtra = extra, comment = Nothing}
gcodeLineToRaw (GCodeLine Nothing extra (Just c)) = RawGCodeLine {cmd = Nothing, rawExtra = extra, comment = Just c}
gcodeLineToRaw (GCodeLine (Just cmd) extra mComment) = RawGCodeLine {cmd = Just (gcodeToRaw cmd), rawExtra = extra, comment = mComment}

gcodeToComment :: GCodeCmd -> Text
gcodeToComment cmd =
  case cmd of
    GMillimeterUnits -> "Set units to millimeters"
    GInchUnits -> "Set units to inches"
    GLinearMove (LinearMove x y z e f) ->
      "Linear move to "
        <> maybe "" (\v -> "X" <> show v) x
        <> maybe "" (\v -> " Y" <> show v) y
        <> maybe "" (\v -> " Z" <> show v) z
        <> maybe "" (\v -> " E" <> show v) e
        <> maybe "" (\v -> " F" <> show v) f
    GAutoHome _ -> "Auto home axes"
    MSetBedTemperature (SetBedTemperature t) ->
      "Set bed temperature to "
        <> maybe "" (\v -> "S" <> show v) t
    MWaitForBedTemperature (WaitForBedTemperature t) ->
      "Wait for bed temperature to reach "
        <> maybe "" (\v -> "S" <> show v) t
    MSSetHotendTemperature (SetHotendTemperature t) ->
      "Set hotend temperature to "
        <> maybe "" (\v -> "S" <> show v) t
    MWaitForHotendTemperature (WaitForHotendTemperature t) ->
      "Wait for hotend temperature to reach "
        <> maybe "" (\v -> "S" <> show v) t

gcodeToRaw :: GCodeCmd -> RawGCodeCmd
gcodeToRaw cmd =
  case cmd of
    GMillimeterUnits ->
      RawGCodeCmd
        { cmdId = 'G',
          cmdNum = 21,
          cmdArgs = Map.empty
        }
    GInchUnits ->
      RawGCodeCmd
        { cmdId = 'G',
          cmdNum = 20,
          cmdArgs = Map.empty
        }
    GLinearMove (LinearMove x y z e f) ->
      RawGCodeCmd
        { cmdId = 'G',
          cmdNum = 1,
          cmdArgs =
            Map.fromList
              $ catMaybes
                [ ('X',) . ArgDouble <$> x,
                  ('Y',) . ArgDouble <$> y,
                  ('Z',) . ArgDouble <$> z,
                  ('E',) . ArgDouble <$> e,
                  ('F',) . ArgInt <$> f
                ]
        }
    GAutoHome _ ->
      RawGCodeCmd
        { cmdId = 'G',
          cmdNum = 28,
          cmdArgs = Map.empty
        }
    MSetBedTemperature (SetBedTemperature t) ->
      RawGCodeCmd
        { cmdId = 'M',
          cmdNum = 140,
          cmdArgs =
            Map.fromList
              $ catMaybes
                [ ('S',) . ArgInt <$> t
                ]
        }
    MWaitForBedTemperature (WaitForBedTemperature t) ->
      RawGCodeCmd
        { cmdId = 'M',
          cmdNum = 190,
          cmdArgs =
            Map.fromList
              $ catMaybes
                [ ('S',) . ArgInt <$> t
                ]
        }
    MSSetHotendTemperature (SetHotendTemperature t) ->
      RawGCodeCmd
        { cmdId = 'M',
          cmdNum = 104,
          cmdArgs =
            Map.fromList
              $ catMaybes
                [ ('S',) . ArgInt <$> t
                ]
        }
    MWaitForHotendTemperature (WaitForHotendTemperature t) ->
      RawGCodeCmd
        { cmdId = 'M',
          cmdNum = 109,
          cmdArgs =
            Map.fromList
              $ catMaybes
                [ ('S',) . ArgInt <$> t
                ]
        }
