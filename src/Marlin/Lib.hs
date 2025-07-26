module Marlin.Lib where

import Linear (V2 (..), V3 (..))
import Marlin.Core
import Marlin.DSL
import Marlin.Math
import Relude

extrudeTo :: V2 Double -> GCode ()
extrudeTo v1 = do
  env <- ask
  linearMove
    & setXY v1
    & setExtrude 10
    & setSpeed env.extrudeSpeed
    & toGCode

extrude :: Double -> GCode ()
extrude s = do
  env <- ask
  linearMove
    & setExtrude s
    & setSpeed env.extrudeSpeed
    & toGCode

moveTo :: V2 Double -> GCode ()
moveTo v = do
  speed <- getSpeed

  withRetract $ do
    linearMove
      & setXY v
      & setSpeed speed
      & toGCode

getSpeed :: GCode Int
getSpeed = do
  env <- ask
  pure $
    if env.startLayer == 0
      then env.moveSpeedFirstLayer
      else env.moveSpeed

moveTo3d :: V3 Double -> GCode ()
moveTo3d v = do
  speed <- getSpeed

  withRetract $ do
    linearMove
      & setXYZ v
      & setSpeed speed
      & toGCode

withRetract :: GCode a -> GCode a
withRetract inner = do
  let retractLength = 3
  extrude (-retractLength)
  ret <- inner
  extrude retractLength
  pure ret

printManyPolyLines :: [[V2 Double]] -> GCode ()
printManyPolyLines = mapM_ printPolyLine

printPolyLine :: [V2 Double] -> GCode ()
printPolyLine [] = pure ()
printPolyLine (v : vs) = do
  moveTo v
  extrudePoints vs

extrudePoints :: [V2 Double] -> GCode ()
extrudePoints vs = do
  forM_ vs $ \v -> do
    extrudeTo v

printSquare :: V2 Double -> V2 Double -> GCode ()
printSquare v1 s = do
  let v2 = v1 + justX s
  let v3 = v2 + justY s
  let v4 = v3 - justX s
  let v5 = v4 - justY s

  printPolyLine [v1, v2, v3, v4, v5]

printTestStripes :: GCode ()
printTestStripes = do
  let v1 = V2 (-5.0) 0.0
  let v2 = V2 (-5.0) 100.0
  let v3 = V2 10.0 0.0
  let v4 = V2 10.0 100.0

  printManyPolyLines
    [ [v1, v2],
      [v3, v4]
    ]

finalPark :: GCode ()
finalPark = do
  env <- ask

  extrude (-3)
  moveTo3d env.parkingPosition

-- printStripesAlongX :: Int -> V2 Double -> V2 Double -> GCode ()
-- printStripesAlongX n v1 s = do
--   let xs = linspace (v1.x) (v1.x + s.x) n
--   let ys = replicate n v1.y

--   let points = zipWith V2 xs ys

--   undefined

homeOrResume :: GCode ()
homeOrResume = do
  env <- ask

  if env.startLayer == 0
    then do
      section "autoHome" $ do
        autoHome
          & setSkipIfTrusted True
          & toGCode
    else do
      section "Resume" $ do
        setPosition
          & setXYZ env.parkingPosition
          & toGCode

initPrinter :: GCode ()
initPrinter = do
  heatup homeOrResume

heatup :: GCode a -> GCode a
heatup inner = do
  env <- ask
  section "Heatup" $ do
    setBedTemperature
      & setTargetTemperature env.bedTemperature
      & toGCode

    setHotendTemperature
      & setTargetTemperature env.hotendTemperature
      & toGCode

  ret <-
    section "Prepare while waiting" $ do
      inner

  section "Wait for temperatures" $ do
    waitForBedTemperature
      & setTargetTemperature env.bedTemperature
      & toGCode

    waitForHotendTemperature
      & setTargetTemperature env.hotendTemperature
      & toGCode

  pure ret

-- (V2 bedX bedY) (V2 sketchX sketchY)

-- redefineOrigin :: GCode ()
-- redefineOrigin = do
--   env <- ask

--   if env.startLayer == 0
--     then do
--       moveTo3d env.parkingPosition
--     else do
--       pure ()

-- let (V3 bedX bedY _) = env.printSize
-- let (V3 sketchX sketchY _) = env.sketchSize

-- let x = (bedX / 2) - (sketchX / 2)
-- let y = (bedY / 2) - (sketchY / 2)

-- moveTo (V2 x y)

-- linearMove
--   & setXY (V2 x y)
--   & setZ 0.2
--   & toGCode

-- setPosition
--   & setXY (V2 0.0 0.0)
--   & setZ 0.2
--   & toGCode