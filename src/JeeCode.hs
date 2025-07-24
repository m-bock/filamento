module JeeCode where

import Data.Maybe (fromMaybe)

-- | G-code DSL ADT (with Maybe for optional arguments)
data GCode
    = UnitsMM
    | AbsoluteCoords
    | AbsoluteExtrusion
    | SetBedTemp (Maybe Int)
    | WaitBedTemp (Maybe Int)
    | SetNozzleTemp (Maybe Int)
    | WaitNozzleTemp (Maybe Int)
    | HomeAllAxes
    | MoveToZ (Maybe Double) (Maybe Int)
    | MoveToXY (Maybe Double) (Maybe Double) (Maybe Int)
    | SetExtruderPos (Maybe Double)
    | ExtrudeTo (Maybe Double) (Maybe Int)
    | MoveXYExtrude (Maybe Double) (Maybe Double) (Maybe Double) (Maybe Int)
    | ArcMove (Maybe Double) (Maybe Double) (Maybe Double) (Maybe Double) (Maybe Double) (Maybe Int)
    | NozzleOff
    | BedOff
    | MotorsOff
    | Comment String
    | Beep (Maybe Int) (Maybe Int)  -- ^ Beep: frequency (Hz), duration (ms)
    deriving (Show, Eq)

renderGCode :: GCode -> String
renderGCode g = case g of
    UnitsMM -> "G21             ; units = mm"
    AbsoluteCoords -> "G90             ; absolute coords"
    AbsoluteExtrusion -> "M82             ; absolute extrusion"
    SetBedTemp mt -> "M140" ++ optArg "S" mt ++ pad 8 ++ "; bed → " ++ showMaybe mt ++ " °C"
    WaitBedTemp mt -> "M190" ++ optArg "S" mt ++ pad 8 ++ "; wait bed"
    SetNozzleTemp mt -> "M104" ++ optArg "S" mt ++ pad 7 ++ "; nozzle → " ++ showMaybe mt ++ " °C"
    WaitNozzleTemp mt -> "M109" ++ optArg "S" mt ++ pad 7 ++ "; wait nozzle"
    HomeAllAxes -> "G28             ; home all axes"
    MoveToZ mz mf -> "G1" ++ optArg "Z" mz ++ optArg "F" mf ++ pad 2 ++ "; move to first layer height (" ++ showMaybe mz ++ " mm) F" ++ showMaybe mf
    MoveToXY mx my mf -> "G1" ++ optArg "X" mx ++ optArg "Y" my ++ optArg "F" mf
    SetExtruderPos me -> "G92" ++ optArg "E" me
    ExtrudeTo me mf -> "G1" ++ optArg "E" me ++ optArg "F" mf
    MoveXYExtrude mx my me mf -> "G1" ++ optArg "X" mx ++ optArg "Y" my ++ optArg "E" me ++ optArg "F" mf
    ArcMove mx my mi mj me mf -> "G2" ++ optArg "X" mx ++ optArg "Y" my ++ optArg "I" mi ++ optArg "J" mj ++ optArg "E" me ++ optArg "F" mf
    NozzleOff -> "M104 S0         ; turn off nozzle"
    BedOff -> "M140 S0         ; turn off bed"
    MotorsOff -> "M84             ; motors off"
    Comment s -> "; " ++ s
    Beep mfreq mdur -> "M300" ++ optArg "S" mfreq ++ optArg "P" mdur ++ " ; beep"
  where
    optArg :: Show a => String -> Maybe a -> String
    optArg _ Nothing = ""
    optArg prefix (Just v) = " " ++ prefix ++ show v
    showMaybe :: Show a => Maybe a -> String
    showMaybe = maybe "" show
    pad n = replicate n ' '

renderGCodeProgram :: [GCode] -> String
renderGCodeProgram = unlines . map renderGCode
