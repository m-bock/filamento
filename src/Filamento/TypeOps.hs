module Filamento.TypeOps
  ( angleCircle,
    angleCos,
    angleFromProportion,
    angleSin,
    deltaFloor,
    deltaRound,
    freqBeepHigh,
    freqBeepLow,
    freqBeepMid,
    outOfToCountTotal,
    outOfToProportion,
    posFromDelta,
    posFromMm,
    posToMm,
    posMiddle,
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
    v2PosFromMm,
    v2PosDropZ,
    v2PosFromMm,
    v2SizeByMm,
    v3DeltaDropZ,
    v3DeltaFromMm,
    v3DeltaFromV2,
    v3PosFromMm,
  )
where

import Filamento.Classes
import Filamento.Types.Continous.AbsFactor (AbsFactor)
import Filamento.Types.Continous.Factor
import Filamento.Types.Geometry.Line2D
import Filamento.Types.Geometry.Rect2D
import Filamento.Types.Geometry.Square2D
import Filamento.Types.Quantities.Delta
import Filamento.Types.Quantities.Length
import Filamento.Types.Quantities.Position
import Filamento.Types.Trivial
import Linear
import Relude
import Relude.Unsafe (fromJust)

deltaFloor :: Delta -> Count
deltaFloor = fromNat . floor . toMm

deltaRound :: Delta -> Count
deltaRound = fromNat . round . toMm

-------------------------------------------------------------------------------

posFromDelta :: Delta -> Position
posFromDelta d = add (posFromMm 0) d

posToMm :: Position -> Double
posToMm = toMm

posMiddle :: Position -> Position -> Position
posMiddle p1 p2 = add p1 (scale @Factor 0.5 (getDelta p1 p2))

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

v2PosDropZ :: V3 Position -> V2 Position
v2PosDropZ (V3 x y _) = V2 x y

-------------------------------------------------------------------------------

v3PosFromMm :: (Double, Double, Double) -> V3 Position
v3PosFromMm (x, y, z) = V3 (posFromMm x) (posFromMm y) (posFromMm z)

-------------------------------------------------------------------------------

outOfToCountTotal :: OutOf -> (Count, Total)
outOfToCountTotal outOf = (outOfGetCount outOf, outOfGetTotal outOf)

outOfToProportion :: OutOf -> Proportion
outOfToProportion outOf = propFromOutOf outOf

-------------------------------------------------------------------------------

square2FromCenterSize :: V2 Position -> Length -> Square2D
square2FromCenterSize center size = square2FromMinSize (add center (pure @V2 $ scale (fromJust $ maybeFromDouble @AbsFactor 0.5) size)) size

square2ToCenterSize :: Square2D -> (V2 Position, V2 Length)
square2ToCenterSize square = (square2GetCenter square, square2GetSize square)

square2GetCenter :: Square2D -> V2 Position
square2GetCenter square = add (square2GetMinCorner square) (scale (fromJust $ maybeFromDouble @AbsFactor 0.5) (square2GetSize square))

square2GetMaxCorner :: Square2D -> V2 Position
square2GetMaxCorner square = add (square2GetMinCorner square) (square2GetSize square)

square2ToRect2 :: Square2D -> Rect2D
square2ToRect2 (square2ToCenterSize -> (center, size)) = rect2From (RectCenter center, RectSize size)

squareGetLines :: Square2D -> (Line2D, Line2D, Line2D, Line2D)
squareGetLines (square2ToRect2 -> rect) = (line1, line2, line3, line4)
  where
    (RectFrontLeft p1, RectFrontRight p2, RectBackRight p3, RectBackLeft p4) = rect2To rect
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
angleFromProportion (toDouble -> f) = angleFromRad (f * 2 * pi)

-------------------------------------------------------------------------------

v2PosFromMm :: (Double, Double) -> V2 Position
v2PosFromMm (x, y) = V2 (posFromMm x) (posFromMm y)

v2SizeByMm :: (Double, Double) -> V2 Length
v2SizeByMm (x, y) = V2 (lengthByMm x) (lengthByMm y)