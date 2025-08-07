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
    GAutoHome (AutoHome _skipIfTrusted) ->
      RawGCodeCmd
        { cmdId = 'G',
          cmdNum = 28,
          cmdArgs =
            Map.fromList
              []
        }
    -- ('O', ArgFlag _skipIfTrusted)

    GSetPosition (SetPosition x y z e) ->
      RawGCodeCmd
        { cmdId = 'G',
          cmdNum = 92,
          cmdArgs =
            Map.fromList
              $ catMaybes
                [ ('X',) . ArgDouble <$> x,
                  ('Y',) . ArgDouble <$> y,
                  ('Z',) . ArgDouble <$> z,
                  ('E',) . ArgDouble <$> e
                ]
        }
    M140SetBedTemperature (SetBedTemperature t) ->
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
    MSetExtruderRelative ->
      RawGCodeCmd
        { cmdId = 'M',
          cmdNum = 83,
          cmdArgs = Map.empty
        }
    MSetExtruderAbsolute ->
      RawGCodeCmd
        { cmdId = 'M',
          cmdNum = 82,
          cmdArgs = Map.empty
        }
    MSetHotendOff ->
      RawGCodeCmd
        { cmdId = 'M',
          cmdNum = 104,
          cmdArgs = Map.fromList [('S', ArgInt 0)]
        }
    MSetBedOff ->
      RawGCodeCmd
        { cmdId = 'M',
          cmdNum = 140,
          cmdArgs = Map.fromList [('S', ArgInt 0)]
        }
    MSetFanOff ->
      RawGCodeCmd
        { cmdId = 'M',
          cmdNum = 107,
          cmdArgs = Map.empty
        }
    MMotorsOff ->
      RawGCodeCmd
        { cmdId = 'M',
          cmdNum = 84,
          cmdArgs = Map.empty
        }
    MPlayTone (PlayTone f d) ->
      RawGCodeCmd
        { cmdId = 'M',
          cmdNum = 300,
          cmdArgs =
            Map.fromList
              $ catMaybes
                [ ('F',) . ArgInt <$> f,
                  ('D',) . ArgInt <$> d
                ]
        }
    GPause s ->
      RawGCodeCmd
        { cmdId = 'G',
          cmdNum = 4,
          cmdArgs = Map.fromList [('S', ArgInt s)]
        }

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
      { _x = Nothing,
        _y = Nothing,
        _z = Nothing,
        _e = Nothing,
        _f = Nothing
      }
