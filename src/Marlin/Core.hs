{-# LANGUAGE TupleSections #-}

module Marlin.Core where

-- See: https://marlinfw.org/meta/gcode/

import qualified Data.Map.Strict as Map
import Marlin.Syntax
import Relude

data LinearMove = LinearMove
  { x :: Maybe Double,
    y :: Maybe Double,
    z :: Maybe Double,
    extrude :: Maybe Double,
    feedrate :: Maybe Int
  }
  deriving (Show, Eq)

data SetBedTemperature = SetBedTemperature
  { sDegrees :: Maybe Int
  }
  deriving (Show, Eq)

data WaitForBedTemperature = WaitForBedTemperature
  { sDegrees :: Maybe Int
  }
  deriving (Show, Eq)

data SetHotendTemperature = SetHotendTemperature
  { sDegrees :: Maybe Int
  }
  deriving (Show, Eq)

data WaitForHotendTemperature = WaitForHotendTemperature
  { sDegrees :: Maybe Int
  }
  deriving (Show, Eq)

data AutoHome = AutoHome
  { _skipIfTrusted :: Bool
  }
  deriving (Show, Eq)

data SetPosition = SetPosition
  { _x :: Maybe Double,
    _y :: Maybe Double,
    _z :: Maybe Double,
    _e :: Maybe Double
  }
  deriving (Show, Eq)

data PlayTone = PlayTone
  { _frequency :: Maybe Int,
    _duration :: Maybe Int
  }
  deriving (Show, Eq)

data GCodeCmd
  = GMillimeterUnits
  | GInchUnits
  | GLinearMove LinearMove
  | GAutoHome AutoHome
  | GSetPosition SetPosition
  | M140SetBedTemperature SetBedTemperature
  | MWaitForBedTemperature WaitForBedTemperature
  | MSSetHotendTemperature SetHotendTemperature
  | MWaitForHotendTemperature WaitForHotendTemperature
  | MSetExtruderRelative
  | MSetExtruderAbsolute
  | MSetHotendOff
  | MSetBedOff
  | MSetFanOff
  | MMotorsOff
  | MPlayTone PlayTone
  | GPause Int -- G4 with seconds parameter
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
      "Linear move to"
        <> maybe "" (\v -> " X" <> show v) x
        <> maybe "" (\v -> " Y" <> show v) y
        <> maybe "" (\v -> " Z" <> show v) z
        <> maybe "" (\v -> " E" <> show v) e
        <> maybe "" (\v -> " F" <> show v) f
    GAutoHome _ -> "Auto home axes"
    GSetPosition (SetPosition x y z e) ->
      "Set position to"
        <> maybe "" (\v -> " X" <> show v) x
        <> maybe "" (\v -> " Y" <> show v) y
        <> maybe "" (\v -> " Z" <> show v) z
        <> maybe "" (\v -> " E" <> show v) e
    M140SetBedTemperature (SetBedTemperature t) ->
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
    MSetExtruderRelative -> "Set extruder to relative mode"
    MSetExtruderAbsolute -> "Set extruder to absolute mode"
    MSetHotendOff -> "Turn hotend off"
    MSetBedOff -> "Turn bed off"
    MSetFanOff -> "Turn fan off"
    MMotorsOff -> "Turn all motors off"
    MPlayTone (PlayTone f d) -> "Play tone at frequency " <> show f <> " for " <> show d <> " milliseconds"
    GPause s -> "Pause for " <> show s <> " seconds"

gcodeToRaw :: GCodeCmd -> RawGCodeCmd
gcodeToRaw cmd =
  case cmd of
    GMillimeterUnits ->
      RawGCodeCmd 'G' 21 Map.empty
    GInchUnits ->
      RawGCodeCmd 'G' 20 Map.empty
    GLinearMove (LinearMove x y z e f) ->
      RawGCodeCmd 'G' 1
        $ Map.fromList
        $ catMaybes
          [ ('X',) . ArgDouble <$> x,
            ('Y',) . ArgDouble <$> y,
            ('Z',) . ArgDouble <$> z,
            ('E',) . ArgDouble <$> e,
            ('F',) . ArgInt <$> f
          ]
    GAutoHome (AutoHome _skipIfTrusted) ->
      RawGCodeCmd 'G' 28 $ Map.fromList []
    GSetPosition (SetPosition x y z e) ->
      RawGCodeCmd 'G' 92
        $ Map.fromList
        $ catMaybes
          [ ('X',) . ArgDouble <$> x,
            ('Y',) . ArgDouble <$> y,
            ('Z',) . ArgDouble <$> z,
            ('E',) . ArgDouble <$> e
          ]
    M140SetBedTemperature (SetBedTemperature t) ->
      RawGCodeCmd 'M' 140 $ Map.fromList $ catMaybes [('S',) . ArgInt <$> t]
    MWaitForBedTemperature (WaitForBedTemperature t) ->
      RawGCodeCmd 'M' 190 $ Map.fromList $ catMaybes [('S',) . ArgInt <$> t]
    MSSetHotendTemperature (SetHotendTemperature t) ->
      RawGCodeCmd 'M' 104 $ Map.fromList $ catMaybes [('S',) . ArgInt <$> t]
    MWaitForHotendTemperature (WaitForHotendTemperature t) ->
      RawGCodeCmd 'M' 109 $ Map.fromList $ catMaybes [('S',) . ArgInt <$> t]
    MSetExtruderRelative ->
      RawGCodeCmd 'M' 83 Map.empty
    MSetExtruderAbsolute ->
      RawGCodeCmd 'M' 82 Map.empty
    MSetHotendOff ->
      RawGCodeCmd 'M' 104 $ Map.fromList [('S', ArgInt 0)]
    MSetBedOff ->
      RawGCodeCmd 'M' 140 $ Map.fromList [('S', ArgInt 0)]
    MSetFanOff ->
      RawGCodeCmd 'M' 107 Map.empty
    MMotorsOff ->
      RawGCodeCmd 'M' 84 Map.empty
    MPlayTone (PlayTone f d) ->
      RawGCodeCmd 'M' 300
        $ Map.fromList
        $ catMaybes
          [ ('F',) . ArgInt <$> f,
            ('D',) . ArgInt <$> d
          ]
    GPause s ->
      RawGCodeCmd 'G' 4 $ Map.fromList [('S', ArgInt s)]

class GCodeCmdOptsDefault a where
  gcodeDef :: a

instance GCodeCmdOptsDefault SetBedTemperature where
  gcodeDef =
    SetBedTemperature
      { sDegrees = Nothing
      }

instance GCodeCmdOptsDefault WaitForBedTemperature where
  gcodeDef =
    WaitForBedTemperature
      { sDegrees = Nothing
      }

instance GCodeCmdOptsDefault SetHotendTemperature where
  gcodeDef =
    SetHotendTemperature
      { sDegrees = Nothing
      }

instance GCodeCmdOptsDefault WaitForHotendTemperature where
  gcodeDef =
    WaitForHotendTemperature
      { sDegrees = Nothing
      }

instance GCodeCmdOptsDefault AutoHome where
  gcodeDef =
    AutoHome
      { _skipIfTrusted = False
      }

instance GCodeCmdOptsDefault SetPosition where
  gcodeDef =
    SetPosition
      { _x = Nothing,
        _y = Nothing,
        _z = Nothing,
        _e = Nothing
      }

instance GCodeCmdOptsDefault PlayTone where
  gcodeDef =
    PlayTone
      { _frequency = Nothing,
        _duration = Nothing
      }

instance GCodeCmdOptsDefault LinearMove where
  gcodeDef =
    LinearMove
      { x = Nothing,
        y = Nothing,
        z = Nothing,
        extrude = Nothing,
        feedrate = Nothing
      }
