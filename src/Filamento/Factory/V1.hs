module Filamento.Factory.V1 where

import Data.List (elemIndex, nub)
import qualified Debug.Trace as Trace
import Filamento
import Filamento.Math (deltaLinspaceByStep, itemsWithNext, linspace2ByStep, linspaceByStep)
import Linear
import Relude

data Profile = Hill | Valley deriving (Show)

testCode :: GCode ()
testCode = do
  printSolidRect (rect2FromCenterSize (v2PosFromMm 0 0) (fromMm $ V2 6 10)) (fromMm 0.4)

(|>) :: (Show a) => a -> String -> a
x |> msg = Trace.trace (msg ++ ": " ++ show x) x

printSolidRect :: Rect2D -> Delta -> GCode ()
printSolidRect frame lineWidth = do
  let V2 frameX2 frameY2 = rect2GetMaxCorner frame
      V2 frameX1 frameY1 = rect2GetMinCorner frame
      spansX = getSpans frameX1 frameX2
      spansY = getSpans frameY1 frameY2

  forM_ (reverse $ zip spansX spansY) $ \((startX, endX), (startY, endY)) -> do
    let start = V2 startX startY
        end = V2 endX endY
    if (startX == endX) || (startY == endY)
      then
        printLine (line2FromPoints start end)
      else
        printRect' (rect2FromCorners start end)
  where
    getSpans start end =
      let ticks = linspaceByStep start end lineWidth deltaFloor
          ticksCentered = map (\(pos1, pos2) -> posMiddle pos1 pos2) $ itemsWithNext ticks
          count = ceiling (fromIntegral (length ticksCentered) / 2)
       in take count $ zip ticksCentered (reverse ticksCentered)

splitMiddle :: [a] -> ([a], [a])
splitMiddle xs = splitAt n xs
  where
    n = length xs `div` 2

printRect' :: Rect2D -> GCode ()
printRect' rect = section "Print Rect" $ do
  let (frontLeft, frontRight, backRight, backLeft) = rect2GetPoints rect

      lines = [(frontLeft, frontRight), (frontRight, backRight), (backRight, backLeft), (backLeft, frontLeft)]

  forM_ lines $ \(p1, p2) -> do
    printLine (line2FromPoints p1 p2)

printLine :: Line2D -> GCode ()
printLine line = do
  let start = line2GetStart line
      end = line2GetEnd line
      pts = case configDefault.splitLinesEvery of
        Nothing -> [start, end]
        Just val -> case linspace2ByStep start end val deltaRound of
          [] -> [start, end]
          vals -> vals

  forM_ (itemsWithNext pts) $ \(p1, p2) -> do
    moveTo p1
    extrudeTo p2

data Config = Config
  { overlap :: Delta,
    filamentDia :: Delta,
    splitLinesEvery :: Maybe Delta
  }

configDefault :: Config
configDefault =
  Config
    { overlap = fromMm 2,
      filamentDia = fromMm 1.75,
      splitLinesEvery = Nothing -- Just $ fromMm 3
    }

getLength :: OutOf -> Profile -> Delta -> Delta
getLength outOf profile len =
  let frac = case profile of
        Hill -> 1 - (outOfToFraction outOf)
        Valley -> outOfToFraction outOf

      ov = configDefault.overlap
      _ramp = scale (frac + frac) ov
   in (scale (frac + frac) ov) + (-ov) + len + (-ov) + (scale (frac + frac) ov)

getWidth :: Proportion -> Delta
getWidth prop =
  let ang = scale 0.5 $ angleFromProportion prop
      pos = angleSin ang
      delta = getDelta pos 0
   in configDefault.filamentDia -- delta * configDefault.filamentDia * 0.5

printProfile :: Profile -> Position -> Delta -> GCode ()
printProfile profile posY len = do
  let rectCenter = addDelta (V2 0 posY) (V2 20 (scale 0.5 len))

  resetLayers

  local (\env -> env {zHop = scale 1.2 configDefault.filamentDia}) do
    withRetract $ withZHop $ moveTo rectCenter

  printLayers \outOf -> do
    st <- gcodeStateGet
    env <- ask
    let V3 _ _ z = toMm st.currentPosition
        V3 _ _ zMax = toMm env.sketchSize
        prop = fromFraction (z / zMax)
    let size = V2 (getWidth prop) (getLength outOf profile len)
    printSolidRect (rect2FromCenterSize rectCenter size) env.lineWidth

data SpiralConfig = SpiralConfig
  { center :: V3 Position,
    radius :: Delta,
    constant :: Delta
  }

defaultSpiralConfig :: SpiralConfig
defaultSpiralConfig =
  SpiralConfig
    { center = v3PosFromMm 110 110 0,
      radius = fromMm 100,
      constant = fromMm (-0.5)
    }

translateSpiral :: SpiralConfig -> V3 Position -> V3 Position
translateSpiral spiralConfig pos = v3PosFromMm x' y' z
  where
    V3 centerX centerY _ = toMm spiralConfig.center
    V3 x y z = toMm pos

    -- Use the simple fnApprox approach that works
    arcLength = y -- y coordinate represents arc length along spiral
    spiralConstant = toMm spiralConfig.constant -- Increased magnitude for faster inward spiral
    baseRadius = toMm spiralConfig.radius -- x offset from tube radius

    -- Simple approximation: angle = arcLength / averageRadius
    -- For small spiral constants, this works very well
    averageRadius = baseRadius + spiralConstant * (arcLength / (2 * baseRadius))
    angle' = arcLength / averageRadius

    -- -- Calculate actual spiral radius at this angle
    spiralRadius = baseRadius + spiralConstant * angle'

    -- Convert to world coordinates
    m = 3 * (pi / 2) -- Starting angle offset
    finalAngle = m + angle'

    -- Calculate the spiral position
    x' = centerX + (spiralRadius + x) * cos finalAngle
    y' = centerY + (spiralRadius + x) * sin finalAngle

-- Add x offset along the spiral's tangent direction
-- x' = spiralX + x * cos (finalAngle + pi / 2) -- Perpendicular to radius
-- y' = spiralY + x * sin (finalAngle + pi / 2) -- Perpendicular to radius

printFilament :: [FilamentSection] -> GCode ()
printFilament secs = local
  ( \env ->
      env
        { sketchSize = fromMm $ V3 10 10 (toMm configDefault.filamentDia),
          lineWidth = fromMm 0.4,
          layerHeight = fromMm 0.2,
          hotendTemperature = fromCelsius 205,
          bedTemperature = fromCelsius 65,
          moveSpeed = fromMmPerSec 2000,
          extrudeSpeed = fromMmPerSec 2500,
          retractLength = fromMm 1.5,
          transpose = translateSpiral defaultSpiralConfig
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
                sdist = getDelta begin end
                colorIndex = fromMaybe 0 (elemIndex sec.color colors)
            setTool colorIndex
            comment ("Print " <> show (sec, secPrev, begin, end, sdist))
            printProfile profile begin sdist

        filamentChange