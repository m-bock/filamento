module Filamento.IO where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Filamento.Core
import Filamento.Types
import Relude

saveGCodeToFile :: String -> GCode () -> (GCodeEnv -> GCodeEnv) -> IO ()
saveGCodeToFile fileName gcode mkEnv = do
  let env = mkEnv defaultGCodeEnv
  let gcode' = local mkEnv gcode
  let (_, st, codeStr) = runGcode gcode' env initPrintState
  writeFileLBS "out/print-report.json" $ encodePretty (reverse st.filament)
  writeFileText fileName codeStr

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
