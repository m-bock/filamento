{-# LANGUAGE TupleSections #-}

module Marlin.Core
  ( GCodeCmd (..),
    GCodeLine (..),
    gcodeLineToRaw,
    gcodeToRaw,
    gcodeToComment,
    GCodeCmdOptsDefault (..),
    LinearMove (..),
    SetBedTemperature (..),
    WaitForBedTemperature (..),
    SetHotendTemperature (..),
    WaitForHotendTemperature (..),
    AutoHome (..),
    SetPosition (..),
    PlayTone (..),
    Pause (..),
    Dwell (..),
  )
where

-- See: https://marlinfw.org/meta/gcode/

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Marlin.Syntax
import Relude
import Text.Printf (printf)

data LinearMove = LinearMove
  { x :: Maybe Double,
    y :: Maybe Double,
    z :: Maybe Double,
    extrude :: Maybe Double,
    feedrate :: Maybe Int
  }
  deriving (Show, Eq, Generic)

data SetBedTemperature = SetBedTemperature
  { sDegrees :: Maybe Int
  }
  deriving (Show, Eq, Generic)

data WaitForBedTemperature = WaitForBedTemperature
  { sDegrees :: Maybe Int
  }
  deriving (Show, Eq, Generic)

data SetHotendTemperature = SetHotendTemperature
  { sDegrees :: Maybe Int
  }
  deriving (Show, Eq, Generic)

data WaitForHotendTemperature = WaitForHotendTemperature
  { sDegrees :: Maybe Int
  }
  deriving (Show, Eq, Generic)

data AutoHome = AutoHome
  { _skipIfTrusted :: Bool
  }
  deriving (Show, Eq, Generic)

data SetPosition = SetPosition
  { _x :: Maybe Double,
    _y :: Maybe Double,
    _z :: Maybe Double,
    _e :: Maybe Double
  }
  deriving (Show, Eq, Generic)

data PlayTone = PlayTone
  { _frequency :: Maybe Int,
    _duration :: Maybe Int
  }
  deriving (Show, Eq, Generic)

data Pause = Pause
  { _seconds :: Maybe Int
  }
  deriving (Show, Eq, Generic)

data Dwell = Dwell
  { seconds :: Maybe Int
  }
  deriving (Show, Eq, Generic)

data GCodeCmd
  = GMillimeterUnits
  | GInchUnits
  | GLinearMove LinearMove
  | GAutoHome AutoHome
  | GSetPosition SetPosition
  | MSetBedTemperature SetBedTemperature
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
  | GDwell Dwell
  deriving (Show, Eq, Generic)

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
        <> maybe "" (\v -> " X" <> printNum v) x
        <> maybe "" (\v -> " Y" <> printNum v) y
        <> maybe "" (\v -> " Z" <> printNum v) z
        <> maybe "" (\v -> " E" <> printNum v) e
        <> maybe "" (\v -> " F" <> show v) f
    GAutoHome _ -> "Auto home axes"
    GSetPosition (SetPosition x y z e) ->
      "Set position to"
        <> maybe "" (\v -> " X" <> show v) x
        <> maybe "" (\v -> " Y" <> printNum v) y
        <> maybe "" (\v -> " Z" <> printNum v) z
        <> maybe "" (\v -> " E" <> printNum v) e
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
    MSetExtruderRelative -> "Set extruder to relative mode"
    MSetExtruderAbsolute -> "Set extruder to absolute mode"
    MSetHotendOff -> "Turn hotend off"
    MSetBedOff -> "Turn bed off"
    MSetFanOff -> "Turn fan off"
    MMotorsOff -> "Turn all motors off"
    MPlayTone (PlayTone f d) -> "Play tone at frequency " <> show f <> " for " <> show d <> " milliseconds"
    GDwell (Dwell s) -> "Dwell for " <> show s <> " seconds"

printNum :: Double -> Text
printNum = T.pack . printf "%.5f"

gcodeToRaw :: GCodeCmd -> RawGCodeCmd
gcodeToRaw cmd =
  case cmd of
    GMillimeterUnits ->
      RawGCodeCmd "G21" Map.empty
    GInchUnits ->
      RawGCodeCmd "G20" Map.empty
    GLinearMove (LinearMove x y z e f) ->
      RawGCodeCmd "G1"
        $ Map.fromList
        $ catMaybes
          [ ('X',) . ArgDouble <$> x,
            ('Y',) . ArgDouble <$> y,
            ('Z',) . ArgDouble <$> z,
            ('E',) . ArgDouble <$> e,
            ('F',) . ArgInt <$> f
          ]
    GAutoHome (AutoHome _skipIfTrusted) ->
      RawGCodeCmd "G28" $ Map.fromList []
    GSetPosition (SetPosition x y z e) ->
      RawGCodeCmd "G92"
        $ Map.fromList
        $ catMaybes
          [ ('X',) . ArgDouble <$> x,
            ('Y',) . ArgDouble <$> y,
            ('Z',) . ArgDouble <$> z,
            ('E',) . ArgDouble <$> e
          ]
    MSetBedTemperature (SetBedTemperature t) ->
      RawGCodeCmd "M140" $ Map.fromList $ catMaybes [('S',) . ArgInt <$> t]
    MWaitForBedTemperature (WaitForBedTemperature t) ->
      RawGCodeCmd "M190" $ Map.fromList $ catMaybes [('S',) . ArgInt <$> t]
    MSSetHotendTemperature (SetHotendTemperature t) ->
      RawGCodeCmd "M104" $ Map.fromList $ catMaybes [('S',) . ArgInt <$> t]
    MWaitForHotendTemperature (WaitForHotendTemperature t) ->
      RawGCodeCmd "M109" $ Map.fromList $ catMaybes [('S',) . ArgInt <$> t]
    MSetExtruderRelative ->
      RawGCodeCmd "M83" Map.empty
    MSetExtruderAbsolute ->
      RawGCodeCmd "M82" Map.empty
    MSetHotendOff ->
      RawGCodeCmd "M104" $ Map.fromList [('S', ArgInt 0)]
    MSetBedOff ->
      RawGCodeCmd "M140" $ Map.fromList [('S', ArgInt 0)]
    MSetFanOff ->
      RawGCodeCmd "M107" Map.empty
    MMotorsOff ->
      RawGCodeCmd "M84" Map.empty
    MPlayTone (PlayTone f d) ->
      RawGCodeCmd "M300"
        $ Map.fromList
        $ catMaybes
          [ ('F',) . ArgInt <$> f,
            ('D',) . ArgInt <$> d
          ]
    GDwell (Dwell s) ->
      RawGCodeCmd "G4" $ Map.fromList $ catMaybes [('S',) . ArgInt <$> s]

class GCodeCmdOptsDefault a where
  gcodeDef :: a

instance GCodeCmdOptsDefault SetBedTemperature where
  gcodeDef = SetBedTemperature Nothing

instance GCodeCmdOptsDefault WaitForBedTemperature where
  gcodeDef = WaitForBedTemperature Nothing

instance GCodeCmdOptsDefault SetHotendTemperature where
  gcodeDef =
    SetHotendTemperature Nothing

instance GCodeCmdOptsDefault WaitForHotendTemperature where
  gcodeDef =
    WaitForHotendTemperature Nothing

instance GCodeCmdOptsDefault AutoHome where
  gcodeDef =
    AutoHome False

instance GCodeCmdOptsDefault SetPosition where
  gcodeDef = SetPosition Nothing Nothing Nothing Nothing

instance GCodeCmdOptsDefault PlayTone where
  gcodeDef = PlayTone Nothing Nothing

instance GCodeCmdOptsDefault LinearMove where
  gcodeDef = LinearMove Nothing Nothing Nothing Nothing Nothing

instance GCodeCmdOptsDefault Pause where
  gcodeDef = Pause Nothing

instance GCodeCmdOptsDefault Dwell where
  gcodeDef = Dwell Nothing