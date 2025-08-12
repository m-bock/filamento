module Filamento.Factory.V1 where

import Data.List (elemIndex, nub, (!!))
import Filamento
import Filamento.Math (linspaceByStepLength)
import Linear
import Relude

data Profile = Hill | Valley deriving (Show)

splitInterval :: Delta -> Delta -> (Delta, Int)
splitInterval big small =
  let n = round (big / small)
      d = big / fromIntegral n
   in (d, n)

printSnakeByCenter :: Position2D -> Delta2D -> GCode ()
printSnakeByCenter center delta = do
  let deltaHalf = scale 0.5 delta
      frontLeft = subDelta center deltaHalf
      frontRight = addDelta frontLeft (justX delta)
      backRight = addDelta frontRight (justY delta)

  printSnake frontLeft backRight

printSnake :: Position2D -> Position2D -> GCode ()
printSnake frontLeft backRight = section "Print Snake" $ do
  env <- ask
  let size = backRight - frontLeft
      frontRight = frontLeft + justX size
      backLeft = backRight - justX size

      V2 w _ = toMm size

  let (doubleStep, count) = splitInterval (deltaFromMm (abs w)) (scale 2 env.lineWidth)

  let step = doubleStep / 2

  forM_ [0 .. count - 1] \i -> do
    let di = fromIntegral i
        plus = (di + 0.5) * step
        minus = -plus
        frontLeft' = addDelta frontLeft (delta2FromDelta plus plus)
        frontRight' = addDelta frontRight (delta2FromDelta minus plus)
        backRight' = addDelta backRight (delta2FromDelta minus minus)
        backLeft' = addDelta backLeft (delta2FromDelta plus minus)

    moveTo frontLeft'

    local (\e -> e {lineWidth = step}) do
      section ("Snake " <> show i) do
        section "Front" do
          extrudeTo frontRight'
        section "Right" do
          extrudeTo backRight'
        section "Back" do
          extrudeTo backLeft'
        section "Left" do
          extrudeTo frontLeft'

printRectByCenter :: Position2D -> Delta2D -> GCode ()
printRectByCenter center delta = do
  let deltaHalf = scale 0.5 delta
      p1 = subDelta center deltaHalf
      p2 = addDelta p1 (justX delta)
      p3 = addDelta p2 (justY delta)
      p4 = subDelta p3 (justX delta)

  moveTo p1
  extrudeTo p2
  extrudeTo p3
  extrudeTo p4
  extrudeTo p1

data Config = Config
  { overlap :: Delta,
    filamentDia :: Delta
  }

configDefault :: Config
configDefault =
  Config
    { overlap = fromMm 0, -- 5.0,
      filamentDia = fromMm 1.75
    }

getLength :: OutOf -> Profile -> Delta -> Delta
getLength c profile len =
  let baseLen = configDefault.overlap + len + configDefault.overlap
   in baseLen

getWidth :: OutOf -> Delta
getWidth _ = fromMm 100

printProfile :: Profile -> Position -> Delta -> GCode ()
printProfile profile posY len = do
  let rectCenter = addDelta (pos2FromPos 0 posY) (delta2FromDelta mempty (scale 0.5 len))

  resetLayers

  printLayers \outOf -> do
    let size = delta2FromDelta configDefault.filamentDia (getLength outOf profile len)
    printSnakeByCenter rectCenter size

printFilament :: [FilamentSection] -> GCode ()
printFilament secs = local
  ( \env ->
      env
        { sketchSize = delta3FromDelta 10 10 configDefault.filamentDia,
          lineWidth = fromMm 0.4,
          layerHeight = fromMm 0.2,
          hotendTemperature = fromCelsius 205,
          bedTemperature = fromCelsius 65,
          moveSpeed = fromMmPerSec 2000,
          extrudeSpeed = fromMmPerSec 2500,
          retractLength = fromMm 1.5
        }
  )
  do
    let secsWithIndex = zip [0 ..] secs
        evens = filter (even . fst) secsWithIndex
        odds = filter (odd . fst) secsWithIndex

    let colors = map (\x -> x.color) secs & nub

    forM_ [(Hill, evens), (Valley, odds)] $ \(profile, items) -> do
      section ("Profile " <> show profile) do
        forM_ items $ \(i, sec) -> do
          section ("Segment " <> show i) do
            let secPrev = secs !!? (i - 1)
                begin = maybe 0 (\x -> x.endPosMm) secPrev
                end = sec.endPosMm
                sdist = signedDistance begin end
                colorIndex = fromMaybe 0 (elemIndex sec.color colors)
            setTool colorIndex
            printProfile profile begin sdist