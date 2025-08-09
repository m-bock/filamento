module Marlin.Core
  ( GCodeCmd (..),
    GCodeLine (..),
    gcodeLineToRaw,
    gcodeToRaw,
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
  { degrees :: Maybe Int
  }
  deriving (Show, Eq, Generic)

data WaitForBedTemperature = WaitForBedTemperature
  { degrees :: Maybe Int
  }
  deriving (Show, Eq, Generic)

data SetHotendTemperature = SetHotendTemperature
  { degrees :: Maybe Int
  }
  deriving (Show, Eq, Generic)

data WaitForHotendTemperature = WaitForHotendTemperature
  { degrees :: Maybe Int
  }
  deriving (Show, Eq, Generic)

data AutoHome = AutoHome
  { skipIfTrusted :: Bool
  }
  deriving (Show, Eq, Generic)

data SetPosition = SetPosition
  { x :: Maybe Double,
    y :: Maybe Double,
    z :: Maybe Double,
    extrude :: Maybe Double
  }
  deriving (Show, Eq, Generic)

data PlayTone = PlayTone
  { frequency :: Maybe Int,
    milliseconds :: Maybe Int
  }
  deriving (Show, Eq, Generic)

data Pause = Pause
  { seconds :: Maybe Int
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
          [ ('S',) . ArgInt <$> f,
            ('P',) . ArgInt <$> d
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