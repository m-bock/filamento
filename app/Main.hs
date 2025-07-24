module Main where

import JeeCode

-- General initial phase for any print
initialPhase :: [GCode]
initialPhase =
    [ UnitsMM
    , AbsoluteCoords
    , AbsoluteExtrusion
    , Comment "—heat & home (adjust temps as desired)—"
    , SetBedTemp 60
    , WaitBedTemp 60
    , SetNozzleTemp 200
    , WaitNozzleTemp 200
    --, HomeAllAxes
    , Comment "—move up to first layer height—"
    -- , MoveToZ 0.2 1200
    ]

testStripePhase :: [GCode]
testStripePhase =
    [ Comment "—purge/wipe line at side—"
    , Comment "Move to purge start at X10,Y10"
    , MoveToXY 10 10 3000
    , Comment "Extrude while moving to X100,Y10"
    , MoveXYExtrude 100 10 10 1000
    , Comment "Retract slightly"
    , ExtrudeTo 8 300
    ]

circleStartPhase :: [GCode]
circleStartPhase =
    [ Comment "—go to circle start point (150,100)—"
    --, MoveToXY 150 100 3000
    , Comment "—prime a bit—"
    , SetExtruderPos 0
    , ExtrudeTo 5 300
    ]

finalPhase :: [GCode]
finalPhase =
    [ Beep 2000 1000
    , Comment "—finish—"
    , NozzleOff
    , BedOff
    , MotorsOff
    ]

sampleProgram :: [GCode]
sampleProgram =
    [ Beep 1000 500 ] ++
    
    initialPhase ++
    [

        MoveToXY 0 0 3000,
        MoveToXY 100 100 3000
    ] ++
    -- circleStartPhase ++
    -- testStripePhase ++
    -- [ Comment "—draw CW circle of R=50 mm around center (100,100)—"
    -- , Comment "start at (150,100), I = –50, J = 0"
    -- , ArcMove 150 100 (-50) 0 314.16 1000
    -- ] ++
    [ Beep 2000 500 ] 
    -- finalPhase

main :: IO ()
main = putStrLn (renderGCodeProgram sampleProgram) 