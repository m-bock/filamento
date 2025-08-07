{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Marlin.DSL
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
  )
where

import Control.Monad.Writer
import Linear (V2 (..), V3 (..))
import Marlin.Core
import Relude
import System.Random

-------------------------------------------------------------------------------
--- GCode
-------------------------------------------------------------------------------

data GCodeEnv = Env
  { moveSpeed :: Int,
    extrudeSpeed :: Int,
    moveSpeedFirstLayer :: Int,
    extrudeSpeedFirstLayer :: Int,
    bedTemperature :: Int,
    hotendTemperature :: Int,
    printSize :: V3 Double,
    parkingPosition :: V3 Double,
    sketchSize :: V3 Double,
    autoHomePosition :: V3 Double,
    layerHeight :: Double,
    firstLayerHeight :: Double,
    lineWidth :: Double,
    filamentDia :: Double,
    transpose :: V2 Double,
    retractLength :: Double,
    retractSpeed :: Int,
    zHop :: Double
  }

data PrintState = PrintState
  { currentLayer :: Int,
    currentPosition :: V3 Double,
    stdgen :: StdGen,
    filamentExtruded :: Double
  }
  deriving (Show, Eq)

initPrintState :: PrintState
initPrintState =
  PrintState
    { currentPosition = V3 145.50 94.00 1.56,
      filamentExtruded = 0,
      stdgen = mkStdGen 0,
      currentLayer = 0
    }

defaultGCodeEnv :: GCodeEnv
defaultGCodeEnv =
  Env
    { moveSpeed = 10000,
      extrudeSpeed = 2000,
      moveSpeedFirstLayer = 1000,
      extrudeSpeedFirstLayer = 800,
      bedTemperature = 60,
      hotendTemperature = 210,
      printSize = V3 225 225 280,
      parkingPosition = V3 0 225 120,
      sketchSize = V3 100 100 100,
      autoHomePosition = V3 145.50 94.00 1.56,
      layerHeight = 0.4,
      firstLayerHeight = 0.2,
      lineWidth = 0.4,
      filamentDia = 1.75,
      transpose = V2 0 0,
      retractLength = 1,
      retractSpeed = 1800,
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

updatePos :: V3 (Maybe Double) -> GCode ()
updatePos (V3 mx my mz) = GCode $ do
  st <- get
  let (V3 x y z) = st.currentPosition
      newPos = V3 (fromMaybe x mx) (fromMaybe y my) (fromMaybe z mz)
  put $ st {currentPosition = newPos}

updateExtruded :: Maybe Double -> GCode ()
updateExtruded extruded = GCode $ do
  st <- get
  let newExtruded = st.filamentExtruded + fromMaybe 0 extruded
  put $ st {filamentExtruded = newExtruded}

setExtruded :: Maybe Double -> GCode ()
setExtruded extruded = GCode $ do
  st <- get
  for_ extruded $ \e -> do
    put $ st {filamentExtruded = e}

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

-------------------------------------------------------------------------------
--- Utils
-------------------------------------------------------------------------------

gCodeFromCmd :: GCodeCmd -> GCode ()
gCodeFromCmd cmd = do
  case cmd of
    GMillimeterUnits -> pure ()
    GInchUnits -> pure ()
    GLinearMove opt -> do
      updatePos (V3 opt.x opt.y opt.z)
      updateExtruded opt.extrude
    GAutoHome _ -> do
      env <- ask
      updatePos (fmap Just env.autoHomePosition)
    GSetPosition opt -> do
      updatePos (V3 opt.x opt.y opt.z)
      setExtruded opt.extrude
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

  GCode
    $ tell
    $ pure
    $ GCodeLine
      { cmd = Just cmd,
        rawExtra = "",
        comment = Just (gcodeToComment cmd)
      }
