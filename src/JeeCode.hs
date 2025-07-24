module JeeCode where

-- | G-code DSL ADT (refactored with descriptive constructors)
data GCode
    = UnitsMM
    | AbsoluteCoords
    | AbsoluteExtrusion
    | SetBedTemp Int
    | WaitBedTemp Int
    | SetNozzleTemp Int
    | WaitNozzleTemp Int
    | HomeAllAxes
    | MoveToZ Double Int
    | MoveToXY Double Double Int
    | SetExtruderPos Double
    | ExtrudeTo Double Int
    | MoveXYExtrude Double Double Double Int
    | ArcMove Double Double Double Double Double Int
    | NozzleOff
    | BedOff
    | MotorsOff
    | Comment String
    | Beep Int Int  -- ^ Beep: frequency (Hz), duration (ms)
    deriving (Show, Eq)

renderGCode :: GCode -> String
renderGCode g = case g of
    UnitsMM -> "G21             ; units = mm"
    AbsoluteCoords -> "G90             ; absolute coords"
    AbsoluteExtrusion -> "M82             ; absolute extrusion"
    SetBedTemp t -> "M140 S" ++ show t ++ "        ; bed → " ++ show t ++ " °C"
    WaitBedTemp t -> "M190 S" ++ show t ++ "        ; wait bed"
    SetNozzleTemp t -> "M104 S" ++ show t ++ "       ; nozzle → " ++ show t ++ " °C"
    WaitNozzleTemp t -> "M109 S" ++ show t ++ "       ; wait nozzle"
    HomeAllAxes -> "G28             ; home all axes"
    MoveToZ z f -> "G1 Z" ++ show z ++ " F" ++ show f ++ "  ; move to first layer height (" ++ show z ++ " mm) F" ++ show f
    MoveToXY x y f -> "G1 X" ++ show x ++ " Y" ++ show y ++ " F" ++ show f
    SetExtruderPos e -> "G92 E" ++ show e
    ExtrudeTo e f -> "G1 E" ++ show e ++ " F" ++ show f
    MoveXYExtrude x y e f -> "G1 X" ++ show x ++ " Y" ++ show y ++ " E" ++ show e ++ " F" ++ show f
    ArcMove x y i j e f -> "G2 X" ++ show x ++ " Y" ++ show y ++ " I" ++ show i ++ " J" ++ show j ++ " E" ++ show e ++ " F" ++ show f
    NozzleOff -> "M104 S0         ; turn off nozzle"
    BedOff -> "M140 S0         ; turn off bed"
    MotorsOff -> "M84             ; motors off"
    Comment s -> "; " ++ s
    Beep freq dur -> "M300 S" ++ show freq ++ " P" ++ show dur ++ " ; beep"

renderGCodeProgram :: [GCode] -> String
renderGCodeProgram = unlines . map renderGCode
