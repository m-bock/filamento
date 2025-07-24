module Main where

import Marlin.Core
import Marlin.DSL
import Relude

-- -- General initial phase for any print
-- initialPhase :: [GCode]
-- initialPhase =
--     [ SetMillimeterUnits
--     , SetAbsolutePositioning
--     , SetExtruderToAbsolute
--     , Comment "—heat & home (adjust temps as desired)—"
--     , SetBedTemperature (Just 60)
--     , WaitForBedTemperature (Just 60)
--     , SetHotendTemperature (Just 200)
--     , WaitForHotendTemperature (Just 200)
--     --, AutoHome
--     , Comment "—move up to first layer height—"
--     -- , GLinearMove (LinearMove Nothing Nothing (Just 0.2) Nothing (Just 1200))
--     ]

-- testStripePhase :: [GCode]
-- testStripePhase =
--     [ Comment "—purge/wipe line at side—"
--     , Comment "Move to purge start at X10,Y10"
--     , GLinearMove (LinearMove (Just 10) (Just 10) Nothing Nothing (Just 3000))
--     , Comment "Extrude while moving to X100,Y10"
--     , GLinearMove (LinearMove (Just 100) (Just 10) Nothing (Just 10) (Just 1000))
--     , Comment "Retract slightly"
--     , GLinearMove (LinearMove Nothing Nothing Nothing (Just 8) (Just 300))
--     ]

-- circleStartPhase :: [GCode]
-- circleStartPhase =
--     [ Comment "—go to circle start point (150,100)—"
--     --, GLinearMove (LinearMove (Just 150) (Just 100) Nothing Nothing (Just 3000))
--     , Comment "—prime a bit—"
--     , SetPosition (Just 0)
--     , GLinearMove (LinearMove Nothing Nothing Nothing (Just 5) (Just 300))
--     ]

-- finalPhase :: [GCode]
-- finalPhase =
--     [ PlayTone (Just 2000) (Just 1000)
--     , Comment "—finish—"
--     , SetHotendTemperatureOff
--     , SetBedTemperatureOff
--     , DisableSteppers
--     ]

-- sampleProgram :: [GCode]
-- sampleProgram =
--     [ PlayTone (Just 1000) (Just 500) ] ++
--     initialPhase ++
--     [
--         GLinearMove (LinearMove (Just 0) (Just 0) Nothing Nothing (Just 3000)),
--         GLinearMove (LinearMove (Just 100) (Just 100) Nothing Nothing (Just 3000))
--     ] ++
--     -- circleStartPhase ++
--     -- testStripePhase ++
--     -- [ Comment "—draw CW circle of R=50 mm around center (100,100)—"
--     -- , Comment "start at (150,100), I = –50, J = 0"
--     -- , ArcMove (Just 150) (Just 100) (Just (-50)) (Just 0) (Just 314.16) (Just 1000)
--     -- ] ++
--     [ PlayTone (Just 2000) (Just 500) ]
--     -- finalPhase

sampleProgram :: [GCode]
sampleProgram = []

main :: IO ()
main = putTextLn (render sampleProgram)