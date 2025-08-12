module Marlin.Core
  ( GCodeCmd (..),
    GCodeLine (..),
    gcodeLineToRaw,
  )
where

-- See: https://marlinfw.org/meta/gcode/

import qualified Data.Map.Strict as Map
import Marlin.Syntax
import Relude

data GCodeCmd
  = GMillimeterUnits
  | GInchUnits
  | GLinearMove
      { x :: Maybe Double,
        y :: Maybe Double,
        z :: Maybe Double,
        extrude :: Maybe Double,
        feedrate :: Maybe Int
      }
  | GAutoHome {skipIfTrusted :: Bool}
  | GSetPosition
      { x :: Maybe Double,
        y :: Maybe Double,
        z :: Maybe Double,
        extrude :: Maybe Double
      }
  | MSetBedTemperature {degrees :: Maybe Int}
  | MWaitForBedTemperature {degrees :: Maybe Int}
  | MSSetHotendTemperature {degrees :: Maybe Int}
  | MWaitForHotendTemperature {degrees :: Maybe Int}
  | MSetExtruderRelative
  | MSetExtruderAbsolute
  | MSetHotendOff
  | MSetBedOff
  | MSetFanOff
  | MMotorsOff
  | MPlayTone {frequency :: Maybe Int, milliseconds :: Maybe Int}
  | GDwell {seconds :: Maybe Int}
  | MSetFanSpeed {speed :: Maybe Int}
  | GCleanNozzle
  | T0
  | T1
  | T2
  | T3
  | T4
  | T5
  | T6
  | T7
  deriving (Show, Eq, Generic)

data GCodeLine = GCodeLine
  { cmd :: Maybe GCodeCmd,
    rawExtra :: Text,
    comment :: Maybe Text
  }
  deriving (Show, Eq)

gcodeLineToRaw :: GCodeLine -> RawGCodeLine
gcodeLineToRaw (GCodeLine mcmd extra mComment) =
  RawGCodeLine
    { cmd = gcodeToRaw <$> mcmd,
      rawExtra = extra,
      comment = mComment
    }

mkRawGCodeCmd :: Text -> [Maybe (Char, ArgValue)] -> RawGCodeCmd
mkRawGCodeCmd cmd args =
  RawGCodeCmd cmd $ Map.fromList $ catMaybes args

gcodeToRaw :: GCodeCmd -> RawGCodeCmd
gcodeToRaw c =
  case c of
    GMillimeterUnits -> mkRawGCodeCmd "G21" []
    GInchUnits -> mkRawGCodeCmd "G20" []
    GLinearMove x y z e f ->
      mkRawGCodeCmd
        "G1"
        [ ('X',) . ArgDouble <$> x,
          ('Y',) . ArgDouble <$> y,
          ('Z',) . ArgDouble <$> z,
          ('E',) . ArgDouble <$> e,
          ('F',) . ArgInt <$> f
        ]
    GAutoHome _skipIfTrusted ->
      mkRawGCodeCmd "G28" []
    GSetPosition x y z e ->
      mkRawGCodeCmd
        "G92"
        [ ('X',) . ArgDouble <$> x,
          ('Y',) . ArgDouble <$> y,
          ('Z',) . ArgDouble <$> z,
          ('E',) . ArgDouble <$> e
        ]
    MSetBedTemperature t ->
      mkRawGCodeCmd "M140" [('S',) . ArgInt <$> t]
    MWaitForBedTemperature t ->
      mkRawGCodeCmd "M190" [('S',) . ArgInt <$> t]
    MSSetHotendTemperature t ->
      mkRawGCodeCmd "M104" [('S',) . ArgInt <$> t]
    MWaitForHotendTemperature t ->
      mkRawGCodeCmd "M109" [('S',) . ArgInt <$> t]
    MSetExtruderRelative ->
      mkRawGCodeCmd "M83" []
    MSetExtruderAbsolute ->
      mkRawGCodeCmd "M82" []
    MSetHotendOff ->
      mkRawGCodeCmd "M104" [Just ('S', ArgInt 0)]
    MSetBedOff ->
      mkRawGCodeCmd "M140" [Just ('S', ArgInt 0)]
    MSetFanOff ->
      mkRawGCodeCmd "M107" []
    MMotorsOff ->
      mkRawGCodeCmd "M84" []
    MPlayTone f d ->
      mkRawGCodeCmd
        "M300"
        [ ('S',) . ArgInt <$> f,
          ('P',) . ArgInt <$> d
        ]
    GDwell s ->
      mkRawGCodeCmd "G4" [('S',) . ArgInt <$> s]
    MSetFanSpeed speed ->
      mkRawGCodeCmd "M106" [('S',) . ArgInt <$> speed]
    GCleanNozzle ->
      mkRawGCodeCmd "G12" []
    T0 ->
      mkRawGCodeCmd "T0" []
    T1 ->
      mkRawGCodeCmd "T1" []
    T2 ->
      mkRawGCodeCmd "T2" []
    T3 ->
      mkRawGCodeCmd "T3" []
    T4 ->
      mkRawGCodeCmd "T4" []
    T5 ->
      mkRawGCodeCmd "T5" []
    T6 ->
      mkRawGCodeCmd "T6" []
    T7 ->
      mkRawGCodeCmd "T7" []