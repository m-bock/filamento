module Marlin.Comment (gcodeToComment) where

import Marlin.Core
import Relude

gcodeToComment :: GCodeCmd -> Text
gcodeToComment cmd =
  case cmd of
    GMillimeterUnits -> "Set units to millimeters"
    GInchUnits -> "Set units to inches"
    GLinearMove {} -> "Linear move"
    GAutoHome {} -> "Auto home axes"
    GSetPosition {} -> "Set position"
    GCleanNozzle -> "Clean nozzle"
    MSetBedTemperature {} -> "Set bed temperature"
    MWaitForBedTemperature {} -> "Wait for bed temperature"
    MSSetHotendTemperature {} -> "Set hotend temperature"
    MWaitForHotendTemperature {} -> "Wait for hotend temperature"
    MSetExtruderRelative -> "Set extruder to relative mode"
    MSetExtruderAbsolute -> "Set extruder to absolute mode"
    MSetHotendOff -> "Turn hotend off"
    MSetBedOff -> "Turn bed off"
    MSetFanOff -> "Turn fan off"
    MSetFanSpeed {} -> "Set fan speed"
    MMotorsOff -> "Turn all motors off"
    MPlayTone {} -> "Play tone"
    GDwell {} -> "Dwell"
    T0 -> "Select tool 0"
    T1 -> "Select tool 1"
    T2 -> "Select tool 2"
    T3 -> "Select tool 3"
    T4 -> "Select tool 4"
    T5 -> "Select tool 5"
    T6 -> "Select tool 6"
    T7 -> "Select tool 7"
