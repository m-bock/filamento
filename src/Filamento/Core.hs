module Filamento.Core
  ( gcodeFromCmd,
    GCode,
    GCodeEnv (..),
    PrintState (..),
    initPrintState,
    defaultGCodeEnv,
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
  )
where

import Control.Monad.Writer
import qualified Data.Text as Text
import Filamento.Classes
import Filamento.Types
import Linear (V2 (..), V3 (..))
import Marlin.Comment (gcodeToComment)
import Marlin.Core
import Relude
import System.Random

-------------------------------------------------------------------------------
--- GCode
-------------------------------------------------------------------------------

data GCodeEnv = Env
  { moveSpeed :: Speed,
    extrudeSpeed :: Speed,
    moveSpeedFirstLayer :: Speed,
    extrudeSpeedFirstLayer :: Speed,
    bedTemperature :: Temperature,
    hotendTemperature :: Temperature,
    parkingPosition :: Position3D,
    autoHomePosition :: Position3D,
    layerHeight :: Delta,
    firstLayerHeight :: Delta,
    lineWidth :: Delta,
    filamentDia :: Delta,
    transpose :: Position3D -> Position3D,
    retractLength :: Delta,
    retractSpeed :: Speed,
    zHop :: Delta,
    sectionPath :: [Text]
  }

data PrintState = PrintState
  { currentLayer :: Int,
    currentPosition :: Position3D,
    stdgen :: StdGen,
    filament :: [(Text, Delta)]
  }
  deriving (Show, Eq)

initPrintState :: PrintState
initPrintState =
  PrintState
    { currentPosition = fromMm $ V3 0 0 0,
      stdgen = mkStdGen 0,
      currentLayer = 0,
      filament = []
    }

defaultGCodeEnv :: GCodeEnv
defaultGCodeEnv =
  Env
    { moveSpeed = fromMmPerSec 10000,
      extrudeSpeed = fromMmPerSec 2000,
      moveSpeedFirstLayer = fromMmPerSec 1000,
      extrudeSpeedFirstLayer = fromMmPerSec 800,
      bedTemperature = fromCelsius 60,
      hotendTemperature = fromCelsius 210,
      parkingPosition = fromMm $ V3 0 225 120,
      autoHomePosition = fromMm $ V3 145.50 94.00 1.56,
      layerHeight = fromMm 0.4,
      firstLayerHeight = fromMm 0.2,
      lineWidth = fromMm 0.4,
      filamentDia = fromMm 1.75,
      transpose = id,
      retractLength = fromMm 1,
      retractSpeed = fromMmPerMin 1800,
      zHop = fromMm 0.4,
      sectionPath = []
    }

newtype GCode a = GCode (StateT PrintState (ReaderT GCodeEnv (Writer [GCodeLine])) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader GCodeEnv,
      MonadState PrintState
    )

instance ToText (GCode a) where
  toText (GCode m) =
    evalStateT m initPrintState
      & (`runReaderT` defaultGCodeEnv)
      & execWriter
      & map gcodeLineToRaw
      & toText

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
newline = GCode $ tell [GCodeLine {cmd = Nothing, rawExtra = "", comment = Nothing}]

comment :: Text -> GCode ()
comment c = GCode $ do
  tell [GCodeLine {cmd = Nothing, rawExtra = "", comment = Just c}]

raw :: Text -> Text -> GCode ()
raw extra comm = GCode $ do
  tell [GCodeLine {cmd = Nothing, rawExtra = extra, comment = Just comm}]

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
  modify \st -> st {currentPosition = env.autoHomePosition}
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

operateTool :: Position3D -> Speed -> Delta -> GCode ()
operateTool v_ speed extr = do
  env <- ask

  modify \st -> st {currentPosition = v_}

  let V3 mx my mz = toMm $ env.transpose v_

  gcodeFromCmd
    $ GLinearMove
      { x = Just mx,
        y = Just my,
        z = Just mz,
        feedrate = Just $ round $ toMmPerSec speed,
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
  operateTool (pos3fromMm newX newY newZ) speed 0

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

extrude :: Speed -> Delta -> GCode ()
extrude speed extr = do
  v <- getCurrentPosition
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

getExtrudeLength :: Position3D -> GCode Delta
getExtrudeLength target = do
  extrudeMM <- getExtrudeMM
  st <- get
  let lineLength = toMm $ pos3Distance st.currentPosition target
  pure (fromMm $ lineLength * extrudeMM)

getExtrudeMM :: GCode Double
getExtrudeMM = do
  env <- ask
  let vPerMm = toMm env.layerHeight * toMm env.lineWidth
      aFil = pi * (toMm env.filamentDia ** 2) / 4
  pure (vPerMm / aFil)

isFirstLayers :: GCode Bool
isFirstLayers = do
  st <- get
  let (V3 _ _ z) = toMm st.currentPosition
  pure (z <= 0.4)

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
playTone_ = playTone (fromHz 2600) (fromMs 1)

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

setPositionXYZ :: Position3D -> GCode ()
setPositionXYZ (toMm -> V3 x y z) = do
  gcodeFromCmd
    $ GSetPosition
      { x = Just x,
        y = Just y,
        z = Just z,
        extrude = Nothing
      }

setPositionXY :: Position2D -> GCode ()
setPositionXY (toMm -> V2 x y) = do
  gcodeFromCmd
    $ GSetPosition
      { x = Just x,
        y = Just y,
        z = Nothing,
        extrude = Nothing
      }

_changeColor :: Text -> GCode ()
_changeColor colorName = do
  comment ("Change color to: " <> colorName)
  modify \st -> st {filament = (colorName, 0) : st.filament}

data FilamentStrategy = FilamentChange | PreparedFilament
  deriving (Show, Eq)

getCurrentPosition :: GCode Position3D
getCurrentPosition = do
  st <- get
  pure st.currentPosition

gcodeFromCmd :: GCodeCmd -> GCode ()
gcodeFromCmd cmd = do
  env <- ask
  GCode
    $ tell
    $ pure
    $ GCodeLine
      { cmd = Just cmd,
        rawExtra = "",
        comment = Just (toTextSectionPath env.sectionPath <> gcodeToComment cmd)
      }

class MoveTo a where
  moveTo :: a -> GCode ()

instance MoveTo Position3D where
  moveTo (toMm -> V3 x y z) = moveToImpl (Just x) (Just y) (Just z)

instance MoveTo Position2D where
  moveTo (toMm -> V2 x y) = moveToImpl (Just x) (Just y) Nothing

class MoveBy a where
  moveBy :: a -> GCode ()

instance MoveBy Delta3D where
  moveBy (toMm -> V3 x y z) = moveByImpl (Just x) (Just y) (Just z)

instance MoveBy Delta2D where
  moveBy (toMm -> V2 x y) = moveByImpl (Just x) (Just y) Nothing

class ExtrudeTo a where
  extrudeTo :: a -> GCode ()

instance ExtrudeTo Position3D where
  extrudeTo (toMm -> V3 x y z) = extrudeToImpl (Just x) (Just y) (Just z)

instance ExtrudeTo Position2D where
  extrudeTo (toMm -> V2 x y) = extrudeToImpl (Just x) (Just y) Nothing

class ExtrudeBy a where
  extrudeBy :: a -> GCode ()

instance ExtrudeBy Delta3D where
  extrudeBy (toMm -> V3 x y z) = extrudeByImpl (Just x) (Just y) (Just z)

instance ExtrudeBy Delta2D where
  extrudeBy (toMm -> V2 x y) = extrudeByImpl (Just x) (Just y) Nothing
