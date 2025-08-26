module Filamento.IO where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.List.NonEmpty as NE
import Filamento.Core
import Filamento.TypeOps
import Relude

-- data OutputConfig = OutputConfig
--   { gcodeFile :: FilePath,
--     reportFile :: FilePath,
--     gcode :: GCode (),
--     env :: GCodeEnv
--   }

data GCodeItf = GCodeItf
  { emitGCode :: GCode ()
  }

mkGCodeItfCatFile :: FilePath -> GCodeItf
mkGCodeItfCatFile = undefined

mkGCodeItfSilent :: GCode () -> GCodeItf
mkGCodeItfSilent = undefined

mkGCodeItfOctoAPI :: GCode () -> GCodeItf
mkGCodeItfOctoAPI = undefined

-- generateGcode :: OutputConfig -> IO ()
-- generateGcode OutputConfig {gcodeFile, reportFile, gcode, env} = do
--   let gcode' = local (const env) gcode
--   (_, st) <- gcodeRun gcode' env (gcodeStateInit env)

--   writeFileLBS reportFile $ encodePretty (NE.reverse st.filament)
--   writeFileText gcodeFile $ toText st.gCode

data PrintReport = PrintReport
  { gcodeFile :: FilePath,
    colors :: [(Text, Delta)]
  }
  deriving (Show, Eq, Generic)

instance ToJSON PrintReport

instance FromJSON PrintReport

data PersistentState = PersistentState
  {count :: Int}
  deriving (Show, Eq, Generic)

instance FromJSON PersistentState

instance ToJSON PersistentState

readPersistentState :: IO PersistentState
readPersistentState = do
  let persistentFile = "persistent-state.json"
  c <- readFileBS persistentFile
  v <- case decodeStrict c of
    Just x -> pure x
    Nothing -> error "Failed to decode printing-state.json"

  let v' = v {count = v.count + 1}
  encodeFile persistentFile v'

  pure v
