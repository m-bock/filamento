module Main where

import JeeCode

-- General initial phase for any print
initialPhase :: [GCode]
initialPhase =
    [ UnitsMM
    , AbsoluteCoords
    , AbsoluteExtrusion
    , Comment "—heat & home (adjust temps as desired)—"
    , SetBedTemp (Just 60)
    , WaitBedTemp (Just 60)
    , SetNozzleTemp (Just 200)
    , WaitNozzleTemp (Just 200)
    --, HomeAllAxes
    , Comment "—move up to first layer height—"
    -- , MoveToZ (Just 0.2) (Just 1200)
    ]

testStripePhase :: [GCode]
testStripePhase =
    [ Comment "—purge/wipe line at side—"
    , Comment "Move to purge start at X10,Y10"
    , MoveToXY (Just 10) (Just 10) (Just 3000)
    , Comment "Extrude while moving to X100,Y10"
    , MoveXYExtrude (Just 100) (Just 10) (Just 10) (Just 1000)
    , Comment "Retract slightly"
    , ExtrudeTo (Just 8) (Just 300)
    ]

circleStartPhase :: [GCode]
circleStartPhase =
    [ Comment "—go to circle start point (150,100)—"
    --, MoveToXY (Just 150) (Just 100) (Just 3000)
    , Comment "—prime a bit—"
    , SetExtruderPos (Just 0)
    , ExtrudeTo (Just 5) (Just 300)
    ]

finalPhase :: [GCode]
finalPhase =
    [ Beep (Just 2000) (Just 1000)
    , Comment "—finish—"
    , NozzleOff
    , BedOff
    , MotorsOff
    ]

sampleProgram :: [GCode]
sampleProgram =
    [ Beep (Just 1000) (Just 500) ] ++
    initialPhase ++
    [
        MoveToXY (Just 0) (Just 0) (Just 3000),
        MoveToXY (Just 100) (Just 100) (Just 3000)
    ] ++
    -- circleStartPhase ++
    -- testStripePhase ++
    -- [ Comment "—draw CW circle of R=50 mm around center (100,100)—"
    -- , Comment "start at (150,100), I = –50, J = 0"
    -- , ArcMove (Just 150) (Just 100) (Just (-50)) (Just 0) (Just 314.16) (Just 1000)
    -- ] ++
    [ Beep (Just 2000) (Just 500) ] 
    -- finalPhase

main :: IO ()
main = putStrLn (renderGCodeProgram sampleProgram) 