module Filamento.Core
  ( gcodeFromCmd,
    GCode,
    GCodeEnv (..),
    GCodeState (..),
    gcodeStateInit,
    gcodeEnvDefault,
    section,
    newline,
    comment,
    raw,
    rand,
    setUnits,
    Units (..),
    autoHome,
    setExtruderRelative,
    setExtruderAbsolute,
    setHotendOff,
    setBedOff,
    setFanOff,
    motorsOff,
    pause,
    moveByZ,
    moveToX,
    moveToY,
    moveToZ,
    moveByX,
    moveByY,
    extrudeByX,
    extrudeByY,
    extrudeByZ,
    extrudeToX,
    extrudeToY,
    extrudeToZ,
    extrude,
    playTone,
    playTone_,
    setBedTemperature,
    setHotendTemperature,
    waitForBedTemperature,
    waitForHotendTemperature,
    setPositionXYZ,
    setPositionXY,
    getCurrentPosition,
    setFanSpeed,
    setFanSpeedOff,
    setFanSpeedFull,
    MoveTo (..),
    MoveBy (..),
    ExtrudeTo (..),
    ExtrudeBy (..),
    withSketchTranspose,
    gcodeRun,
    FilamentSection (..),
    gcodeStateGet,
    nextLayer,
    setTool,
    resetLayers,
    changeColor,
    incLayers,
    decLayers,
  )
where

import Control.Monad.Writer
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Data.Convertible (convert)
import Data.List (elemIndex)
-- import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Filamento.Classes
import Filamento.TypeOps
import Linear (V2 (..), V3 (..))
import Marlin.Comment (gcodeToComment)
import Marlin.Core
import Relude
import System.Random

-------------------------------------------------------------------------------
--- GCode
-------------------------------------------------------------------------------

newtype GCode a = GCode (StateT GCodeState (ReaderT GCodeEnv IO) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader GCodeEnv,
      MonadIO
    )

gcodeRun :: GCode a -> GCodeEnv -> GCodeState -> IO (a, GCodeState)
gcodeRun (GCode m) env oldState = do
  val <-
    runStateT m oldState
      & (`runReaderT` env)
  pure val

gcodeFromCmd :: GCodeCmd -> GCode ()
gcodeFromCmd cmd = do
  env <- ask
  gcodeStateModify $ MsgAddGCodeLine GCodeLine {cmd = Just cmd, rawExtra = "", comment = Just (toTextSectionPath env.sectionPath <> gcodeToComment cmd)}

-------------------------------------------------------------------------------
--- GCodeEnv
-------------------------------------------------------------------------------

data GCodeEnv = Env
  { moveSpeed :: Speed,
    extrudeSpeed :: Speed,
    moveSpeedFirstLayer :: Speed,
    extrudeSpeedFirstLayer :: Speed,
    bedTemperature :: Temperature,
    hotendTemperature :: Temperature,
    parkingPosition :: V3 Position,
    autoHomePosition :: V3 Position,
    sketchSize :: V3 Delta,
    layerHeight :: Delta,
    firstLayerHeight :: Delta,
    lineWidth :: Delta,
    filamentDia :: Delta,
    transpose :: V3 Position -> V3 Position,
    retractLength :: Delta,
    retractSpeed :: Speed,
    zHop :: Delta,
    sectionPath :: [Text],
    bedSize :: V2 Delta,
    colors :: NonEmpty Text,
    emitGCode :: GCode ()
  }

gcodeEnvDefault :: GCodeEnv
gcodeEnvDefault =
  Env
    { moveSpeed = fromMmPerSec 150,
      extrudeSpeed = fromMmPerSec 35,
      moveSpeedFirstLayer = fromMmPerSec 17,
      extrudeSpeedFirstLayer = fromMmPerSec 13,
      bedTemperature = fromCelsius 60,
      hotendTemperature = fromCelsius 210,
      parkingPosition = fromMm $ V3 0 225 120,
      autoHomePosition = fromMm $ V3 145.50 94.00 1.56,
      layerHeight = fromMm 0.2,
      firstLayerHeight = fromMm 0.2,
      lineWidth = fromMm 0.4,
      filamentDia = fromMm 1.75,
      sketchSize = fromMm $ V3 100 100 100,
      transpose = id,
      retractLength = fromMm 1,
      retractSpeed = fromMmPerSec 30,
      zHop = fromMm 0.4,
      sectionPath = [],
      bedSize = fromMm $ V2 225 225,
      colors = "default" :| [],
      emitGCode = pure ()
    }

-------------------------------------------------------------------------------
--- FilamentSection
-------------------------------------------------------------------------------

data FilamentSection = FilamentSection
  { color :: Text,
    endPosMm :: Position
  }
  deriving (Show, Eq, Generic)

instance FromJSON FilamentSection

instance ToJSON FilamentSection

-------------------------------------------------------------------------------
--- GCodeState
-------------------------------------------------------------------------------

data GCodeState = GCodeState
  { currentLayer :: Int,
    currentPosition :: V3 Position,
    stdgen :: StdGen,
    filament :: NonEmpty FilamentSection,
    gCode :: [GCodeLine]
  }
  deriving (Show, Eq)

data Msg
  = MsgChangeCurrentPosition (V3 Position)
  | MsgTrackExtrusion Delta
  | MsgChangeCurrentLayer Int
  | MsgChangeColor Text
  | MsgAddGCodeLine GCodeLine

gcodeStateUpdate :: Msg -> GCodeState -> GCodeState
gcodeStateUpdate msg st = case msg of
  MsgChangeCurrentPosition pos -> st {currentPosition = pos}
  MsgChangeCurrentLayer layer -> st {currentLayer = layer}
  MsgChangeColor color -> case st.filament of
    h :| t ->
      st
        { filament = FilamentSection {color, endPosMm = h.endPosMm} :| h : t
        }
  MsgTrackExtrusion extr -> case st.filament of
    h :| t ->
      st
        { filament = h {endPosMm = addDelta h.endPosMm extr} :| t
        }
  MsgAddGCodeLine line -> st {gCode = st.gCode <> [line]}

gcodeStateModify :: Msg -> GCode ()
gcodeStateModify msg = GCode do
  modify \st -> gcodeStateUpdate msg st

gcodeStateGet :: GCode GCodeState
gcodeStateGet = GCode $ get

gcodeStateInit :: GCodeEnv -> GCodeState
gcodeStateInit env =
  GCodeState
    { currentPosition = fromMm $ V3 0 0 0,
      stdgen = mkStdGen 0,
      currentLayer = 0,
      filament = FilamentSection {color = head env.colors, endPosMm = 0} :| []
    }

-------------------------------------------------------------------------------
--- Utils
-------------------------------------------------------------------------------
transposeCenterSketch :: V3 Delta -> V2 Delta -> V3 Position -> V3 Position
transposeCenterSketch sketchSize bedSize pos =
  let halfSketch = scale @Double 0.5 (v3DeltaDropZ sketchSize)
      halfBed = scale @Double 0.5 bedSize
      diff = v3DeltaFromV2 (halfBed - halfSketch) 0
   in addDelta pos diff

withSketchTranspose :: GCode a -> GCode a
withSketchTranspose inner = do
  env <- ask
  let sketchSize = env.sketchSize
  let bedSize = env.bedSize
  let transposeFn = transposeCenterSketch sketchSize bedSize
  local (\env' -> env' {transpose = transposeFn}) inner

-- | Random value for any type that implements `Random`
rand :: (Random a) => GCode a
rand = GCode $ state $ \st ->
  let (value, newGen) = random st.stdgen
      st' = st {stdgen = newGen}
   in (value, st')

-------------------------------------------------------------------------------

section :: Text -> GCode a -> GCode a
section caption gc = do
  local (\env -> env {sectionPath = env.sectionPath <> [caption]}) gc

toTextSectionPath :: [Text] -> Text
toTextSectionPath [] = ""
toTextSectionPath path =
  "[ " <> Text.intercalate " / " path <> " ] "

newline :: GCode ()
newline = gcodeStateModify $ MsgAddGCodeLine GCodeLine {cmd = Nothing, rawExtra = "", comment = Nothing}

comment :: Text -> GCode ()
comment c = gcodeStateModify $ MsgAddGCodeLine GCodeLine {cmd = Nothing, rawExtra = "", comment = Just c}

raw :: Text -> Text -> GCode ()
raw extra comm = gcodeStateModify $ MsgAddGCodeLine GCodeLine {cmd = Nothing, rawExtra = extra, comment = Just comm}

resetLayers :: GCode ()
resetLayers = section "resetLayers" do
  gcodeStateModify $ MsgChangeCurrentLayer 0

incLayers :: GCode ()
incLayers = do
  st <- gcodeStateGet
  gcodeStateModify $ MsgChangeCurrentLayer (st.currentLayer + 1)

decLayers :: GCode ()
decLayers = do
  st <- gcodeStateGet
  gcodeStateModify $ MsgChangeCurrentLayer (st.currentLayer - 1)

nextLayer :: GCode ()
nextLayer = section "nextLayer" do
  env <- ask
  st <- gcodeStateGet

  let z = fromMm (toMm env.firstLayerHeight + convert st.currentLayer * toMm env.layerHeight)

  gcodeStateModify $ MsgChangeCurrentLayer (st.currentLayer + 1)

  moveToZ z

changeColor :: Text -> GCode ()
changeColor color = do
  env <- ask
  let i = fromMaybe 0 (elemIndex color (toList env.colors))
  gcodeStateModify $ MsgChangeColor color
  raw ("T" <> show i) "tool change"

-------------------------------------------------------------------------------

data Units = Millimeter | Inche

setUnits :: Units -> GCode ()
setUnits u = gcodeFromCmd $ case u of
  Millimeter -> GMillimeterUnits
  Inche -> GInchUnits

-------------------------------------------------------------------------------

autoHome :: GCode ()
autoHome = do
  env <- ask
  gcodeStateModify $ MsgChangeCurrentPosition env.autoHomePosition
  gcodeFromCmd $ GAutoHome {skipIfTrusted = False}

-------------------------------------------------------------------------------

setExtruderRelative :: GCode ()
setExtruderRelative = gcodeFromCmd MSetExtruderRelative

setExtruderAbsolute :: GCode ()
setExtruderAbsolute = gcodeFromCmd MSetExtruderAbsolute

setHotendOff :: GCode ()
setHotendOff = gcodeFromCmd MSetHotendOff

setBedOff :: GCode ()
setBedOff = gcodeFromCmd MSetBedOff

setFanOff :: GCode ()
setFanOff = gcodeFromCmd MSetFanOff

motorsOff :: GCode ()
motorsOff = gcodeFromCmd MMotorsOff

pause :: Duration -> GCode ()
pause dur = gcodeFromCmd $ GDwell {seconds = Just $ round $ toSecs dur}

setTool :: Int -> GCode ()
setTool i =
  let toolCmds = [T0, T1, T2, T3, T4, T5, T6, T7]
      i' = i `mod` (length toolCmds)
      cmd = toolCmds !!? i'
   in gcodeFromCmd (fromMaybe T0 cmd)

setFanSpeed :: Proportion -> GCode ()
setFanSpeed prop =
  gcodeFromCmd
    $ MSetFanSpeed
      { speed = Just (round $ toFraction prop * 255)
      }

setFanSpeedOff :: GCode ()
setFanSpeedOff = setFanSpeed (clampFraction 0)

setFanSpeedFull :: GCode ()
setFanSpeedFull = setFanSpeed (clampFraction 1)

--------------------------------------------------------------------------------

operateTool :: V3 Position -> Speed -> Delta -> GCode ()
operateTool v_ speed extr = do
  env <- ask

  gcodeStateModify $ MsgChangeCurrentPosition v_
  gcodeStateModify $ MsgTrackExtrusion extr

  let V3 mx my mz = toMm $ env.transpose v_

  gcodeFromCmd
    $ GLinearMove
      { x = Just mx,
        y = Just my,
        z = Just mz,
        feedrate = Just $ round $ toMmPerMin speed,
        extrude = Just $ coerce $ toMm extr
      }

--------------------------------------------------------------------------------

moveToImpl :: Maybe Double -> Maybe Double -> Maybe Double -> GCode ()
moveToImpl mx my mz = do
  speed <- getSpeed
  cur <- getCurrentPosition
  let V3 x y z = toMm cur
      newX = fromMaybe x mx
      newY = fromMaybe y my
      newZ = fromMaybe z mz
  operateTool (v3PosFromMm newX newY newZ) speed 0

moveToX :: Position -> GCode ()
moveToX (toMm -> x) =
  moveToImpl (Just x) Nothing Nothing

moveToY :: Position -> GCode ()
moveToY (toMm -> y) =
  moveToImpl Nothing (Just y) Nothing

moveToZ :: Position -> GCode ()
moveToZ (toMm -> z) =
  moveToImpl Nothing Nothing (Just z)

--------------------------------------------------------------------------------

moveByImpl :: Maybe Double -> Maybe Double -> Maybe Double -> GCode ()
moveByImpl mx my mz = do
  speed <- getSpeed
  cur <- getCurrentPosition
  let v = fromMm (V3 (fromMaybe 0 mx) (fromMaybe 0 my) (fromMaybe 0 mz))
  operateTool (v + cur) speed 0

moveByX :: Delta -> GCode ()
moveByX (toMm -> x) =
  moveByImpl (Just x) Nothing Nothing

moveByY :: Delta -> GCode ()
moveByY (toMm -> y) =
  moveByImpl Nothing (Just y) Nothing

moveByZ :: Delta -> GCode ()
moveByZ (toMm -> z) =
  moveByImpl Nothing Nothing (Just z)

--------------------------------------------------------------------------------

extrudeToImpl :: Maybe Double -> Maybe Double -> Maybe Double -> GCode ()
extrudeToImpl mx my mz = do
  speed <- getExtrudeSpeed
  (toMm -> V3 curX curY curZ) <- getCurrentPosition
  let v = fromMm (V3 (fromMaybe curX mx) (fromMaybe curY my) (fromMaybe curZ mz))
  extr <- getExtrudeLength v
  operateTool v speed extr

extrudeToX :: Position -> GCode ()
extrudeToX (toMm -> dx) =
  extrudeToImpl (Just dx) Nothing Nothing

extrudeToY :: Position -> GCode ()
extrudeToY (toMm -> dy) =
  extrudeToImpl Nothing (Just dy) Nothing

extrudeToZ :: Position -> GCode ()
extrudeToZ (toMm -> dz) =
  extrudeToImpl Nothing Nothing (Just dz)

-------------------------------------------------------------------------------

extrudeByImpl :: Maybe Double -> Maybe Double -> Maybe Double -> GCode ()
extrudeByImpl mx my mz = do
  speed <- getExtrudeSpeed
  cur <- getCurrentPosition
  let v = fromMm (V3 (fromMaybe 0 mx) (fromMaybe 0 my) (fromMaybe 0 mz))
  let v' = v + cur
  extr <- getExtrudeLength v'
  operateTool v' speed extr

extrudeByX :: Delta -> GCode ()
extrudeByX (toMm -> x) = extrudeByImpl (Just x) Nothing Nothing

extrudeByY :: Delta -> GCode ()
extrudeByY (toMm -> y) = extrudeByImpl Nothing (Just y) Nothing

extrudeByZ :: Delta -> GCode ()
extrudeByZ (toMm -> z) = extrudeByImpl Nothing Nothing (Just z)

-------------------------------------------------------------------------------

extrude :: Delta -> GCode ()
extrude extr = do
  v <- getCurrentPosition
  speed <- getExtrudeSpeed
  operateTool v speed extr

-------------------------------------------------------------------------------
--- Utils
-------------------------------------------------------------------------------

getSpeed :: GCode Speed
getSpeed = do
  b <- isFirstLayers
  env <- ask
  pure
    $ if b
      then env.moveSpeedFirstLayer
      else env.moveSpeed

getExtrudeLength :: V3 Position -> GCode Delta
getExtrudeLength target = do
  extrudeMM <- getExtrudeMM
  st <- gcodeStateGet
  let lineLength = toMm $ getDistance st.currentPosition target
  pure (fromMm $ lineLength * extrudeMM)

getExtrudeMM :: GCode Double
getExtrudeMM = do
  env <- ask
  let vPerMm = toMm env.layerHeight * toMm env.lineWidth
      aFil = pi * (toMm env.filamentDia ** 2) / 4
  pure (vPerMm / aFil)

isFirstLayers :: GCode Bool
isFirstLayers = do
  st <- gcodeStateGet
  env <- ask
  let (V3 _ _ z) = toMm st.currentPosition
  pure (z <= toMm env.firstLayerHeight)

getExtrudeSpeed :: GCode Speed
getExtrudeSpeed = do
  b <- isFirstLayers
  env <- ask
  pure
    $ if b
      then env.extrudeSpeedFirstLayer
      else env.extrudeSpeed

playTone :: Frequency -> Duration -> GCode ()
playTone freq dur =
  gcodeFromCmd
    $ MPlayTone
      { frequency = Just $ round $ toHz freq,
        milliseconds = Just $ round $ toMs dur
      }

playTone_ :: GCode ()
playTone_ = playTone (fromHz 880) (fromMs 500)

setBedTemperature :: Temperature -> GCode ()
setBedTemperature degrees = do
  gcodeFromCmd
    $ MSetBedTemperature
      { degrees = Just $ round $ toCelsius degrees
      }

setHotendTemperature :: Temperature -> GCode ()
setHotendTemperature temp = do
  gcodeFromCmd
    $ MSSetHotendTemperature
      { degrees = Just $ round $ toCelsius temp
      }

waitForBedTemperature :: Temperature -> GCode ()
waitForBedTemperature temp = do
  gcodeFromCmd
    $ MWaitForBedTemperature
      { degrees = Just $ round $ toCelsius temp
      }

waitForHotendTemperature :: Temperature -> GCode ()
waitForHotendTemperature temp = do
  gcodeFromCmd
    $ MWaitForHotendTemperature
      { degrees = Just $ round $ toCelsius temp
      }

setPositionXYZ :: V3 Position -> GCode ()
setPositionXYZ (toMm -> V3 x y z) = do
  gcodeFromCmd
    $ GSetPosition
      { x = Just x,
        y = Just y,
        z = Just z,
        extrude = Nothing
      }

setPositionXY :: V2 Position -> GCode ()
setPositionXY (toMm -> V2 x y) = do
  gcodeFromCmd
    $ GSetPosition
      { x = Just x,
        y = Just y,
        z = Nothing,
        extrude = Nothing
      }

data FilamentStrategy = FilamentChange | PreparedFilament
  deriving (Show, Eq)

getCurrentPosition :: GCode (V3 Position)
getCurrentPosition = do
  st <- gcodeStateGet
  pure st.currentPosition

class MoveTo a where
  moveTo :: a -> GCode ()

instance MoveTo (V3 Position) where
  moveTo (V3 x y z) = moveToImpl (Just $ toMm x) (Just $ toMm y) (Just $ toMm z)

instance MoveTo (V2 Position) where
  moveTo (V2 x y) = moveToImpl (Just $ toMm x) (Just $ toMm y) Nothing

class MoveBy a where
  moveBy :: a -> GCode ()

instance MoveBy (V2 Delta) where
  moveBy (toMm -> V2 x y) = moveByImpl (Just x) (Just y) Nothing

class ExtrudeTo a where
  extrudeTo :: a -> GCode ()

class ExtrudeBy a where
  extrudeBy :: a -> GCode ()

instance ExtrudeTo (V3 Position) where
  extrudeTo (V3 x y z) = extrudeToImpl (Just $ toMm x) (Just $ toMm y) (Just $ toMm z)

instance ExtrudeTo (V2 Position) where
  extrudeTo (V2 x y) = extrudeToImpl (Just $ toMm x) (Just $ toMm y) Nothing

instance ExtrudeBy (V3 Delta) where
  extrudeBy (V3 x y z) = extrudeByImpl (Just $ toMm x) (Just $ toMm y) (Just $ toMm z)

instance ExtrudeBy (V2 Delta) where
  extrudeBy (V2 x y) = extrudeByImpl (Just $ toMm x) (Just $ toMm y) Nothing