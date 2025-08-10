module Marlin.Comment where

import qualified Data.Text as T
import Linear.V3
import Marlin.Core
import Relude
import Text.Printf (printf)

gcodeToComment :: GCodeCmd -> Text
gcodeToComment cmd =
  case cmd of
    GMillimeterUnits -> "Set units to millimeters"
    GInchUnits -> "Set units to inches"
    GLinearMove {} -> "Linear move"
    GAutoHome _ -> "Auto home axes"
    GSetPosition x y z e -> "Set position"
    GCleanNozzle -> "Clean nozzle"
    MSetBedTemperature t -> "Set bed temperature"
    MWaitForBedTemperature t -> "Wait for bed temperature"
    MSSetHotendTemperature t -> "Set hotend temperature"
    MWaitForHotendTemperature t -> "Wait for hotend temperature"
    MSetExtruderRelative -> "Set extruder to relative mode"
    MSetExtruderAbsolute -> "Set extruder to absolute mode"
    MSetHotendOff -> "Turn hotend off"
    MSetBedOff -> "Turn bed off"
    MSetFanOff -> "Turn fan off"
    MSetFanSpeed s -> "Set fan speed"
    MMotorsOff -> "Turn all motors off"
    MPlayTone f d -> "Play tone"
    GDwell s -> "Dwell"

printNum :: Double -> Text
printNum = T.pack . printf "%6.2f"

printV3 :: V3 (Maybe Double) -> Text
printV3 (V3 x y z) = "(" <> printComponent x <> ", " <> printComponent y <> ", " <> printComponent z <> ")"

printComponent :: Maybe Double -> Text
printComponent Nothing = "_"
printComponent (Just v) = printNum v