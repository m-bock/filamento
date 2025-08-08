module Marlin.Comment where

import qualified Data.Text as T
import Linear (V3)
import Linear.V (V)
import Linear.V3
import Marlin.Core
import Relude
import Text.Printf (printf)

gcodeToComment :: GCodeCmd -> Text
gcodeToComment cmd =
  case cmd of
    GMillimeterUnits -> "Set units to millimeters"
    GInchUnits -> "Set units to inches"
    GLinearMove (LinearMove x y z e f) ->
      "Linear move to "
        <> printV3 (V3 x y z)
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
printNum = T.pack . printf "%.2f"

printV3 :: V3 (Maybe Double) -> Text
printV3 (V3 x y z) = "(" <> printComponent x <> ", " <> printComponent y <> ", " <> printComponent z <> ")"

printComponent :: Maybe Double -> Text
printComponent Nothing = "_"
printComponent (Just v) = printNum v