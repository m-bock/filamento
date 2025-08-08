module Filamento
  ( gCodeFromCmd,
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
    Temperature (..),
    Frequency (..),
    Duration (..),
    setPositionXYZ,
    setPositionXY,
    module RePos3D,
  )
where

import Control.Monad.Writer
import Filamento.Types.Displacement2D
import qualified Filamento.Types.Displacement2D as Disp2D
import qualified Filamento.Types.Displacement2D as ReDisp2D (delta2FromMm, delta2ToMm)
import Filamento.Types.Displacement3D
import qualified Filamento.Types.Displacement3D as Disp3D
import Filamento.Types.Distance
import qualified Filamento.Types.Distance as Distance
import Filamento.Types.Duration
import qualified Filamento.Types.Duration as Duration
import Filamento.Types.Frequency
import qualified Filamento.Types.Frequency as Frequency
import Filamento.Types.Position (Position)
import qualified Filamento.Types.Position as Pos
import Filamento.Types.Position2D as Pos2D
import qualified Filamento.Types.Position2D as RePos2D (pos2FromMm, pos2ToMm)
import Filamento.Types.Position3D
import Filamento.Types.Position3D as Pos3D
import qualified Filamento.Types.Position3D as RePos3D (pos3FromMm, pos3ToMm)
import Filamento.Types.Speed as Speed
import Filamento.Types.Temperature as Temperature
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
    printSize :: V3 Double,
    parkingPosition :: V3 Double,
    autoHomePosition :: V3 Double,
    layerHeight :: Double,
    firstLayerHeight :: Double,
    lineWidth :: Double,
    filamentDia :: Double,
    transpose :: Position3D -> Position3D,
    retractLength :: Distance,
    retractSpeed :: Speed,
    zHop :: Double
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
    { currentPosition = Pos3D.fromMm $ V3 145.50 94.00 1.56,
      stdgen = mkStdGen 0,
      currentLayer = 0,
      filament = []
    }

defaultGCodeEnv :: GCodeEnv
defaultGCodeEnv =
  Env
    { moveSpeed = Speed.fromMmPerMin 10000,
      extrudeSpeed = Speed.fromMmPerMin 2000,
      moveSpeedFirstLayer = Speed.fromMmPerMin 1000,
      extrudeSpeedFirstLayer = Speed.fromMmPerMin 800,
      bedTemperature = Temperature.fromCelsius 60,
      hotendTemperature = Temperature.fromCelsius 210,
      printSize = V3 225 225 280,
      parkingPosition = V3 0 225 120,
      autoHomePosition = V3 145.50 94.00 1.56,
      layerHeight = 0.4,
      firstLayerHeight = 0.2,
      lineWidth = 0.4,
      filamentDia = 1.75,
      transpose = id,
      retractLength = 1,
      retractSpeed = Speed.fromMmPerMin 1800,
      zHop = 0.4
    }

newtype GCode a = GCode
  { runGCode :: StateT PrintState (ReaderT GCodeEnv (Writer [GCodeLine])) a
  }
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

-- updatePos :: V3 (Maybe Double) -> GCode ()
-- updatePos (V3 mx my mz) =
--   GCode $ do
--     st <- get
--     let (V3 x y z) = st.currentPosition
--         newPos = V3 (fromMaybe x mx) (fromMaybe y my) (fromMaybe z mz)
--     put $ st {currentPosition = newPos}

-------------------------------------------------------------------------------

section :: Text -> GCode a -> GCode a
section caption gc = do
  newline
  comment caption
  ret <- gc
  newline
  pure ret

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
setUnits u = gCodeFromCmd $ case u of
  Millimeter -> GMillimeterUnits
  Inche -> GInchUnits

-------------------------------------------------------------------------------

autoHome :: GCode ()
autoHome = gCodeFromCmd $ GAutoHome gcodeDef {skipIfTrusted = False}

-------------------------------------------------------------------------------

setExtruderRelative :: GCode ()
setExtruderRelative = gCodeFromCmd MSetExtruderRelative

setExtruderAbsolute :: GCode ()
setExtruderAbsolute = gCodeFromCmd MSetExtruderAbsolute

setHotendOff :: GCode ()
setHotendOff = gCodeFromCmd MSetHotendOff

setBedOff :: GCode ()
setBedOff = gCodeFromCmd MSetBedOff

setFanOff :: GCode ()
setFanOff = gCodeFromCmd MSetFanOff

motorsOff :: GCode ()
motorsOff = gCodeFromCmd MMotorsOff

pause :: Int -> GCode ()
pause seconds = gCodeFromCmd $ GDwell (gcodeDef {seconds = Just seconds})

--------------------------------------------------------------------------------

operateTool :: Position3D -> Speed -> Distance -> GCode ()
operateTool v_ speed extr = do
  env <- ask

  modify \st -> st {currentPosition = v_}

  let V3 mx my mz = Pos3D.toMm $ env.transpose v_

  gCodeFromCmd
    $ GLinearMove
      gcodeDef
        { x = Just mx,
          y = Just my,
          z = Just mz,
          feedrate = Just $ round $ Speed.toMmPerMin speed,
          extrude = Just $ coerce $ Distance.toMm extr
        }

--------------------------------------------------------------------------------

moveToXYZ :: Position3D -> GCode ()
moveToXYZ v = do
  speed <- getSpeed
  operateTool v speed 0

moveToXY :: Position2D -> GCode ()
moveToXY pos = do
  let V2 x y = Pos2D.toMm pos
  speed <- getSpeed
  V3 _ _ z <- Pos3D.toMm <$> getCurrentPosition
  operateTool (Pos3D.fromMm $ V3 x y z) speed 0

moveToX :: Position -> GCode ()
moveToX pos = do
  let x = Pos.toMm pos
  speed <- getSpeed
  V3 _ y z <- Pos3D.toMm <$> getCurrentPosition
  operateTool (Pos3D.fromMm $ V3 x y z) speed 0

moveToY :: Position -> GCode ()
moveToY pos = do
  let y = Pos.toMm pos
  speed <- getSpeed
  V3 x _ z <- Pos3D.toMm <$> getCurrentPosition
  operateTool (Pos3D.fromMm $ V3 x y z) speed 0

moveToZ :: Position -> GCode ()
moveToZ pos = do
  let z = Pos.toMm pos
  speed <- getSpeed
  V3 x y _ <- Pos3D.toMm <$> getCurrentPosition
  operateTool (Pos3D.fromMm $ V3 x y z) speed 0

--------------------------------------------------------------------------------

move :: Distance -> GCode ()
move = undefined

moveXYZ :: Displacement3D -> GCode ()
moveXYZ v = do
  cur <- getCurrentPosition
  speed <- getSpeed
  operateTool (Pos3D.addDisplacement cur v) speed 0

moveXY :: Displacement2D -> GCode ()
moveXY v = do
  cur <- getCurrentPosition
  let V3 x y z = Pos3D.toMm cur
      V2 dx dy = Disp2D.toMm v
  speed <- getSpeed
  operateTool (Pos3D.fromMm $ V3 (x + dx) (y + dy) z) speed 0

moveX :: Double -> GCode ()
moveX x = undefined

moveY :: Double -> GCode ()
moveY y = undefined

moveZ :: Double -> GCode ()
moveZ z = undefined

--------------------------------------------------------------------------------

extrudeToXY :: Position2D -> GCode ()
extrudeToXY v = undefined

extrudeToXYZ :: Position3D -> GCode ()
extrudeToXYZ v = undefined

-- do
-- speed <- getExtrudeSpeed
-- extrudeLength <- getExtrudeLength v
-- operateTool v speed extrudeLength

extrudeToX :: Double -> GCode ()
extrudeToX x = extrudeToXYZ (Pos3D.fromMm $ V3 x 0 0)

extrudeToY :: Double -> GCode ()
extrudeToY y = extrudeToXYZ (Pos3D.fromMm $ V3 0 y 0)

extrudeToZ :: Double -> GCode ()
extrudeToZ z = extrudeToXYZ (Pos3D.fromMm $ V3 0 0 z)

-------------------------------------------------------------------------------

extrudeXYZ :: Displacement3D -> GCode ()
extrudeXYZ v = undefined

-- do
-- v' <- getCurrentPosition
-- extrudeToXYZ (v + v')

extrudeXY :: Displacement2D -> GCode ()
extrudeXY = undefined

-- (V2 x y) = extrudeXYZ (V3 x y 0)

extrudeX :: Double -> GCode ()
extrudeX x = extrudeXYZ (Disp3D.fromMm $ V3 x 0 0)

extrudeY :: Double -> GCode ()
extrudeY y = extrudeXYZ (Disp3D.fromMm $ V3 0 y 0)

extrudeZ :: Double -> GCode ()
extrudeZ z = extrudeXYZ (Disp3D.fromMm $ V3 0 0 z)

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
      then env.moveSpeedFirstLayer
      else env.moveSpeed

getExtrudeLength :: Position3D -> GCode Double
getExtrudeLength target = do
  extrudeMM <- getExtrudeMM
  st <- get
  let lineLength = Distance.toMm $ Pos3D.distance st.currentPosition target
  pure (lineLength * extrudeMM)

getExtrudeMM :: GCode Double
getExtrudeMM = do
  env <- ask
  let vPerMm = env.layerHeight * env.lineWidth
      aFil = pi * (env.filamentDia ^ 2) / 4
  pure (vPerMm / aFil)

isFirstLayers :: GCode Bool
isFirstLayers = do
  st <- get
  let (V3 _ _ z) = Pos3D.toMm st.currentPosition
  pure (z <= 0.4)

getExtrudeSpeed :: GCode Speed
getExtrudeSpeed = do
  b <- isFirstLayers
  env <- ask
  pure
    $ if b
      then env.extrudeSpeedFirstLayer
      else env.extrudeSpeed

stateUpdateExtrude :: Double -> PrintState -> PrintState
stateUpdateExtrude len st = case st.filament of
  [] -> st {filament = [("default", len)]}
  (color, used) : xs -> st {filament = (color, used + len) : xs}

gCodeFromCmd :: GCodeCmd -> GCode ()
gCodeFromCmd cmd = do
  case cmd of
    GMillimeterUnits -> pure ()
    GInchUnits -> pure ()
    GLinearMove opt -> pure ()
    -- updatePos (V3 opt.x opt.y opt.z)
    -- forM_ opt.extrude $ \e -> modify (stateUpdateExtrude e)
    GAutoHome _ -> pure ()
    -- env <- ask
    -- updatePos (fmap Just env.autoHomePosition)
    GSetPosition opt -> pure ()
    -- updatePos (V3 opt.x opt.y opt.z)
    -- setExtruded opt.extrude
    MSetBedTemperature _ -> pure ()
    MWaitForBedTemperature _ -> pure ()
    MSSetHotendTemperature _ -> pure ()
    MWaitForHotendTemperature _ -> pure ()
    MSetExtruderRelative -> pure ()
    MSetExtruderAbsolute -> pure ()
    MSetHotendOff -> pure ()
    MSetBedOff -> pure ()
    MSetFanOff -> pure ()
    MMotorsOff -> pure ()
    MPlayTone _ -> pure ()
    GDwell _ -> pure ()

  let cmd' = case cmd of
        GLinearMove opt -> GLinearMove opt
        _ -> cmd

  GCode
    $ tell
    $ pure
    $ GCodeLine
      { cmd = Just cmd',
        rawExtra = "",
        comment = Just (gcodeToComment cmd)
      }

playTone :: Frequency -> Duration -> GCode ()
playTone freq dur =
  gCodeFromCmd
    $ MPlayTone
      gcodeDef
        { frequency = Just $ round $ Frequency.toHz freq,
          milliseconds = Just $ round $ Duration.toMs dur
        }

playTone_ :: GCode ()
playTone_ = playTone (Frequency.fromHz 2600) (Duration.fromMs 1)

setBedTemperature :: Temperature -> GCode ()
setBedTemperature degrees = do
  gCodeFromCmd
    $ MSetBedTemperature
      gcodeDef
        { degrees = Just $ round $ Temperature.toCelsius degrees
        }

setHotendTemperature :: Temperature -> GCode ()
setHotendTemperature temp = do
  gCodeFromCmd
    $ MSSetHotendTemperature
      gcodeDef
        { degrees = Just $ round $ Temperature.toCelsius temp
        }

waitForBedTemperature :: Temperature -> GCode ()
waitForBedTemperature temp = do
  gCodeFromCmd
    $ MWaitForBedTemperature
      gcodeDef
        { degrees = Just $ round $ Temperature.toCelsius temp
        }

waitForHotendTemperature :: Temperature -> GCode ()
waitForHotendTemperature temp = do
  gCodeFromCmd
    $ MWaitForHotendTemperature
      gcodeDef
        { degrees = Just $ round $ Temperature.toCelsius temp
        }

setPositionXYZ :: V3 Double -> GCode ()
setPositionXYZ (V3 x y z) = do
  gCodeFromCmd
    $ GSetPosition
      gcodeDef
        { x = Just x,
          y = Just y,
          z = Just z
        }

setPositionXY :: V2 Double -> GCode ()
setPositionXY (V2 x y) = do
  gCodeFromCmd
    $ GSetPosition
      gcodeDef
        { x = Just x,
          y = Just y
        }

changeColor :: Text -> GCode ()
changeColor colorName = do
  comment ("Change color to: " <> colorName)
  modify \st -> st {filament = (colorName, 0) : st.filament}

data FilamentStrategy = FilamentChange | PreparedFilament
  deriving (Show, Eq)

getCurrentPosition :: GCode Position3D
getCurrentPosition = do
  st <- get
  pure st.currentPosition