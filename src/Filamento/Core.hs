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
    extrudeXY,
    moveXY,
    moveXYZ,
    moveZ,
    moveToX,
    moveToY,
    moveToZ,
    moveToXY,
    moveToXYZ,
    moveX,
    moveY,
    extrudeX,
    extrudeY,
    extrudeZ,
    extrudeToX,
    extrudeToY,
    extrudeToZ,
    extrudeToXY,
    extrudeToXYZ,
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
  )
where

import Control.Monad.Writer
import qualified Data.Text as Text
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
    movespdFirstLayer :: Speed,
    extrudespdFirstLayer :: Speed,
    bedTemperature :: Temperature,
    hotendTemperature :: Temperature,
    printSize :: V3 Double,
    parkingPosition :: Position3D,
    autoHomePosition :: V3 Double,
    layerHeight :: Double,
    firstLayerHeight :: Double,
    lineWidth :: Double,
    filamentDia :: Double,
    transpose :: Position3D -> Position3D,
    retractLength :: Distance,
    retractSpeed :: Speed,
    zHop :: Double,
    sectionPath :: [Text]
  }

data PrintState = PrintState
  { currentLayer :: Int,
    currentPosition :: Position3D,
    stdgen :: StdGen,
    filament :: [(Text, Double)]
  }
  deriving (Show, Eq)

initPrintState :: PrintState
initPrintState =
  PrintState
    { currentPosition = pos3FromMm $ V3 145.50 94.00 1.56,
      stdgen = mkStdGen 0,
      currentLayer = 0,
      filament = []
    }

defaultGCodeEnv :: GCodeEnv
defaultGCodeEnv =
  Env
    { moveSpeed = spdFromMmPerSec 10000,
      extrudeSpeed = spdFromMmPerSec 2000,
      movespdFirstLayer = spdFromMmPerSec 1000,
      extrudespdFirstLayer = spdFromMmPerSec 800,
      bedTemperature = tempFromCelsius 60,
      hotendTemperature = tempFromCelsius 210,
      printSize = V3 225 225 280,
      parkingPosition = pos3FromMm $ V3 0 225 120,
      autoHomePosition = V3 145.50 94.00 1.56,
      layerHeight = 0.4,
      firstLayerHeight = 0.2,
      lineWidth = 0.4,
      filamentDia = 1.75,
      transpose = id,
      retractLength = 1,
      retractSpeed = spdFromMmPerMin 1800,
      zHop = 0.4,
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
autoHome = gcodeFromCmd $ GAutoHome {skipIfTrusted = False}

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
pause dur = gcodeFromCmd $ GDwell {seconds = Just $ round $ durToSecs dur}

setFanSpeed :: Proportion -> GCode ()
setFanSpeed prop =
  gcodeFromCmd
    $ MSetFanSpeed
      { speed = Just (round $ propToFraction prop * 255)
      }

setFanSpeedOff :: GCode ()
setFanSpeedOff = setFanSpeed (propFromFractionClamped 0)

setFanSpeedFull :: GCode ()
setFanSpeedFull = setFanSpeed (propFromFractionClamped 1)

--------------------------------------------------------------------------------

operateTool :: Position3D -> Speed -> Distance -> GCode ()
operateTool v_ speed extr = do
  env <- ask

  modify \st -> st {currentPosition = v_}

  let V3 mx my mz = pos3ToMm $ env.transpose v_

  gcodeFromCmd
    $ GLinearMove
      { x = Just mx,
        y = Just my,
        z = Just mz,
        feedrate = Just $ round $ spdToMmPerSec speed,
        extrude = Just $ coerce $ distToMm extr
      }

--------------------------------------------------------------------------------

moveToImpl :: Maybe Double -> Maybe Double -> Maybe Double -> GCode ()
moveToImpl mx my mz = do
  speed <- getSpeed
  cur <- getCurrentPosition
  let V3 x y z = pos3ToMm cur
      newX = fromMaybe x mx
      newY = fromMaybe y my
      newZ = fromMaybe z mz
  operateTool (pos3FromMm $ V3 newX newY newZ) speed 0

moveToXYZ :: Position3D -> GCode ()
moveToXYZ (pos3ToMm -> V3 x y z) =
  moveToImpl (Just x) (Just y) (Just z)

moveToXY :: Position2D -> GCode ()
moveToXY (pos2ToMm -> V2 x y) =
  moveToImpl (Just x) (Just y) Nothing

moveToX :: Position -> GCode ()
moveToX (posToMm -> x) =
  moveToImpl (Just x) Nothing Nothing

moveToY :: Position -> GCode ()
moveToY (posToMm -> y) =
  moveToImpl Nothing (Just y) Nothing

moveToZ :: Position -> GCode ()
moveToZ (posToMm -> z) =
  moveToImpl Nothing Nothing (Just z)

--------------------------------------------------------------------------------

moveImpl :: Maybe Double -> Maybe Double -> Maybe Double -> GCode ()
moveImpl mx my mz = do
  speed <- getSpeed
  cur <- getCurrentPosition
  let v = pos3FromMm (V3 (fromMaybe 0 mx) (fromMaybe 0 my) (fromMaybe 0 mz))
  operateTool (v + cur) speed 0

moveXYZ :: Delta3D -> GCode ()
moveXYZ (dlt3ToMm -> V3 dx dy dz) =
  moveImpl (Just dx) (Just dy) (Just dz)

moveXY :: Delta2D -> GCode ()
moveXY (dlt2ToMm -> V2 dx dy) =
  moveImpl (Just dx) (Just dy) Nothing

moveX :: Delta -> GCode ()
moveX (dltToMm -> x) =
  moveImpl (Just x) Nothing Nothing

moveY :: Delta -> GCode ()
moveY (dltToMm -> y) =
  moveImpl Nothing (Just y) Nothing

moveZ :: Delta -> GCode ()
moveZ (dltToMm -> z) =
  moveImpl Nothing Nothing (Just z)

--------------------------------------------------------------------------------

extrudeToImpl :: Maybe Double -> Maybe Double -> Maybe Double -> GCode ()
extrudeToImpl mx my mz = do
  speed <- getExtrudeSpeed
  (pos3ToMm -> V3 curX curY curZ) <- getCurrentPosition
  let v = pos3FromMm (V3 (fromMaybe curX mx) (fromMaybe curY my) (fromMaybe curZ mz))
  extr <- getExtrudeLength v
  operateTool v speed (distFromMm extr)

extrudeToXY :: Position2D -> GCode ()
extrudeToXY (pos2ToMm -> V2 dx dy) =
  extrudeToImpl (Just dx) (Just dy) Nothing

extrudeToXYZ :: Position3D -> GCode ()
extrudeToXYZ (pos3ToMm -> V3 dx dy dz) =
  extrudeToImpl (Just dx) (Just dy) (Just dz)

extrudeToX :: Position -> GCode ()
extrudeToX (posToMm -> dx) =
  extrudeToImpl (Just dx) Nothing Nothing

extrudeToY :: Position -> GCode ()
extrudeToY (posToMm -> dy) =
  extrudeToImpl Nothing (Just dy) Nothing

extrudeToZ :: Position -> GCode ()
extrudeToZ (posToMm -> dz) =
  extrudeToImpl Nothing Nothing (Just dz)

-------------------------------------------------------------------------------

extrudeImpl :: Maybe Double -> Maybe Double -> Maybe Double -> GCode ()
extrudeImpl mx my mz = do
  speed <- getExtrudeSpeed
  cur <- getCurrentPosition
  let v = pos3FromMm (V3 (fromMaybe 0 mx) (fromMaybe 0 my) (fromMaybe 0 mz))
  let v' = v + cur
  extr <- getExtrudeLength v'
  operateTool v' speed (distFromMm extr)

extrudeXYZ :: Delta3D -> GCode ()
extrudeXYZ (dlt3ToMm -> V3 dx dy dz) = do
  extrudeImpl (Just dx) (Just dy) (Just dz)

extrudeXY :: Delta2D -> GCode ()
extrudeXY (dlt2ToMm -> V2 dx dy) = do
  extrudeImpl (Just dx) (Just dy) Nothing

extrudeX :: Delta -> GCode ()
extrudeX (dltToMm -> x) = extrudeXYZ (dlt3FromMm $ V3 x 0 0)

extrudeY :: Delta -> GCode ()
extrudeY (dltToMm -> y) = extrudeXYZ (dlt3FromMm $ V3 0 y 0)

extrudeZ :: Delta -> GCode ()
extrudeZ (dltToMm -> z) = extrudeXYZ (dlt3FromMm $ V3 0 0 z)

-------------------------------------------------------------------------------

extrude :: Speed -> Distance -> GCode ()
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
      then env.movespdFirstLayer
      else env.moveSpeed

getExtrudeLength :: Position3D -> GCode Double
getExtrudeLength target = do
  extrudeMM <- getExtrudeMM
  st <- get
  let lineLength = distToMm $ pos3Distance st.currentPosition target
  pure (lineLength * extrudeMM)

getExtrudeMM :: GCode Double
getExtrudeMM = do
  env <- ask
  let vPerMm = env.layerHeight * env.lineWidth
      aFil = pi * (env.filamentDia ** 2) / 4
  pure (vPerMm / aFil)

isFirstLayers :: GCode Bool
isFirstLayers = do
  st <- get
  let (V3 _ _ z) = pos3ToMm st.currentPosition
  pure (z <= 0.4)

getExtrudeSpeed :: GCode Speed
getExtrudeSpeed = do
  b <- isFirstLayers
  env <- ask
  pure
    $ if b
      then env.extrudespdFirstLayer
      else env.extrudeSpeed

playTone :: Frequency -> Duration -> GCode ()
playTone freq dur =
  gcodeFromCmd
    $ MPlayTone
      { frequency = Just $ round $ freqToHz freq,
        milliseconds = Just $ round $ durToMs dur
      }

playTone_ :: GCode ()
playTone_ = playTone (freqFromHz 2600) (durFromMs 1)

setBedTemperature :: Temperature -> GCode ()
setBedTemperature degrees = do
  gcodeFromCmd
    $ MSetBedTemperature
      { degrees = Just $ round $ tempToCelsius degrees
      }

setHotendTemperature :: Temperature -> GCode ()
setHotendTemperature temp = do
  gcodeFromCmd
    $ MSSetHotendTemperature
      { degrees = Just $ round $ tempToCelsius temp
      }

waitForBedTemperature :: Temperature -> GCode ()
waitForBedTemperature temp = do
  gcodeFromCmd
    $ MWaitForBedTemperature
      { degrees = Just $ round $ tempToCelsius temp
      }

waitForHotendTemperature :: Temperature -> GCode ()
waitForHotendTemperature temp = do
  gcodeFromCmd
    $ MWaitForHotendTemperature
      { degrees = Just $ round $ tempToCelsius temp
      }

setPositionXYZ :: Position3D -> GCode ()
setPositionXYZ (pos3ToMm -> V3 x y z) = do
  gcodeFromCmd
    $ GSetPosition
      { x = Just x,
        y = Just y,
        z = Just z,
        extrude = Nothing
      }

setPositionXY :: Position2D -> GCode ()
setPositionXY (pos2ToMm -> V2 x y) = do
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