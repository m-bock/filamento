module Filamento.TypeOps
  ( angleCircle,
    angleCos,
    angleFromProportion,
    angleSin,
    deltaFloor,
    deltaFromMm,
    deltaMiddle,
    deltaFromPos,
    deltaRound,
    freqBeepHigh,
    freqBeepLow,
    freqBeepMid,
    outOfToCountTotal,
    outOfToFraction,
    outOfToProportion,
    posFromDelta,
    posFromMm,
    posToMm,
    posMiddle,
    rect2FromCenterSize,
    rect2FromCorners,
    rect2GetCenter,
    rect2GetMaxCorner,
    rect2GetPoints,
    rect2GetSize,
    rect2ToCenterSize,
    rect2ToCorners,
    rect2ToMinSize,
    square2FromCenterSize,
    square2GetCenter,
    square2GetMaxCorner,
    square2GetMinCorner,
    square2GetSize,
    square2ToCenterSize,
    square2ToRect2,
    squareGetLines,
    v2DeltaFromMm,
    v2DeltaToV3,
    v2PosDropZ,
    v2PosFromMm,
    v3DeltaDropZ,
    v3DeltaFromMm,
    v3DeltaFromV2,
    v3PosFromMm,
    module Export,
  )
where

import Filamento.Classes
import Filamento.Types as Export
import Linear
import Relude

deltaFromMm :: Double -> Delta
deltaFromMm = fromMm

deltaFloor :: Delta -> Count
deltaFloor = fromInt . floor . toMm

deltaRound :: Delta -> Count
deltaRound = fromInt . round . toMm

deltaFromPos :: Position -> Delta
deltaFromPos pos = getDelta (posFromMm 0) pos

deltaMiddle :: Delta -> Delta -> Delta
deltaMiddle d1 d2 = addDelta d1 (scale 0.5 (getDelta d1 d2))

-------------------------------------------------------------------------------

posFromDelta :: Delta -> Position
posFromDelta d = addDelta (posFromMm 0) d

posToMm :: Position -> Double
posToMm = toMm

posFromMm :: Double -> Position
posFromMm = fromMm

posMiddle :: Position -> Position -> Position
posMiddle p1 p2 = addDelta p1 (scale 0.5 (getDelta p1 p2))

-------------------------------------------------------------------------------

v2DeltaFromMm :: Double -> Double -> V2 Delta
v2DeltaFromMm x y = V2 (fromMm x) (fromMm y)

v2DeltaToV3 :: V2 Delta -> Delta -> V3 Delta
v2DeltaToV3 (V2 x y) z = V3 x y z

-------------------------------------------------------------------------------

v3DeltaFromMm :: Double -> Double -> Double -> V3 Delta
v3DeltaFromMm x y z = V3 (fromMm x) (fromMm y) (fromMm z)

v3DeltaFromV2 :: V2 Delta -> Delta -> V3 Delta
v3DeltaFromV2 (V2 x y) z = V3 x y z

v3DeltaDropZ :: V3 Delta -> V2 Delta
v3DeltaDropZ (V3 x y _) = V2 x y

-------------------------------------------------------------------------------

freqBeepLow :: Frequency
freqBeepLow = fromHz 440 -- A4, common reference pitch

freqBeepMid :: Frequency
freqBeepMid = fromHz 880 -- A5, clear but not harsh

freqBeepHigh :: Frequency
freqBeepHigh = fromHz 1320 -- E6, attention-grabbing but not painful

-------------------------------------------------------------------------------

v2PosFromMm :: Double -> Double -> V2 Position
v2PosFromMm x y = V2 (posFromMm x) (posFromMm y)

v2PosDropZ :: V3 Position -> V2 Position
v2PosDropZ (V3 x y _) = V2 x y

-------------------------------------------------------------------------------

v3PosFromMm :: Double -> Double -> Double -> V3 Position
v3PosFromMm x y z = V3 (posFromMm x) (posFromMm y) (posFromMm z)

-------------------------------------------------------------------------------

outOfToCountTotal :: OutOf -> (Count, Total)
outOfToCountTotal outOf = (outOfGetCount outOf, outOfGetTotal outOf)

outOfToProportion :: OutOf -> Proportion
outOfToProportion (outOfToCountTotal -> (count, total)) = fromFraction (toDouble count / toDouble total)

outOfToFraction :: OutOf -> Double
outOfToFraction (outOfToCountTotal -> (count, total)) = toDouble count / toDouble total

-------------------------------------------------------------------------------

rect2FromCorners :: V2 Position -> V2 Position -> Rect2D
rect2FromCorners minCorner maxCorner =
  rect2FromMinSize minCorner (getDelta minCorner maxCorner)

rect2ToCorners :: Rect2D -> (V2 Position, V2 Position)
rect2ToCorners rect = (rect2GetMinCorner rect, rect2GetMaxCorner rect)

rect2FromCenterSize :: V2 Position -> V2 Delta -> Rect2D
rect2FromCenterSize center size =
  rect2FromMinSize (subDelta center size') size
  where
    size' :: V2 Delta
    size' = scale 0.5 size

rect2ToCenterSize :: Rect2D -> (V2 Position, V2 Delta)
rect2ToCenterSize rect = (rect2GetCenter rect, rect2GetSize rect)

rect2ToMinSize :: Rect2D -> (V2 Position, V2 Delta)
rect2ToMinSize rect = (rect2GetMinCorner rect, rect2GetSize rect)

rect2GetMaxCorner :: Rect2D -> V2 Position
rect2GetMaxCorner (rect2ToMinSize -> (minCorner, size)) = addDelta minCorner size

rect2GetCenter :: Rect2D -> V2 Position
rect2GetCenter (rect2ToMinSize -> (minCorner, size)) = addDelta minCorner (scale 0.5 size)

rect2GetPoints :: Rect2D -> (V2 Position, V2 Position, V2 Position, V2 Position)
rect2GetPoints rect = (p1, p2, p3, p4)
  where
    (minCorner, size) = rect2ToMinSize rect
    p1 = minCorner
    p2 = addDelta p1 (justX size)
    p3 = addDelta p2 (justY size)
    p4 = subDelta p3 (justX size)

-------------------------------------------------------------------------------

square2FromCenterSize :: V2 Position -> Delta -> Square2D
square2FromCenterSize center size = square2FromMinSize (addDelta center (pure $ scale 0.5 size)) size

square2ToCenterSize :: Square2D -> (V2 Position, V2 Delta)
square2ToCenterSize square = (square2GetCenter square, square2GetSize square)

square2GetCenter :: Square2D -> V2 Position
square2GetCenter square = addDelta (square2GetMinCorner square) (scale 0.5 (square2GetSize square))

square2GetMaxCorner :: Square2D -> V2 Position
square2GetMaxCorner square = addDelta (square2GetMinCorner square) (square2GetSize square)

square2ToRect2 :: Square2D -> Rect2D
square2ToRect2 (square2ToCenterSize -> (center, size)) = rect2FromCenterSize center size

squareGetLines :: Square2D -> (Line2D, Line2D, Line2D, Line2D)
squareGetLines (square2ToRect2 -> rect) = (line1, line2, line3, line4)
  where
    (p1, p2, p3, p4) = rect2GetPoints rect
    line1 = line2FromPoints p1 p2
    line2 = line2FromPoints p2 p3
    line3 = line2FromPoints p3 p4
    line4 = line2FromPoints p4 p1

-------------------------------------------------------------------------------

angleSin :: Angle -> Position
angleSin (angleToRad -> a) = fromMm (sin a)

angleCos :: Angle -> Position
angleCos (angleToRad -> a) = fromMm (cos a)

angleCircle :: Angle -> V2 Position
angleCircle ang = V2 (angleCos ang) (angleSin ang)

angleFromProportion :: Proportion -> Angle
angleFromProportion (toFraction -> f) = angleFromRad (f * 2 * pi)