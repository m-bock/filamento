module Filamento.IO where

import Data.Aeson
import Filamento.Core
import Relude

run :: (PersistentState -> (String, GCode ())) -> IO ()
run f = do
  st <- readPersistentState
  let (fileName, gcode) = f st
  let codeStr = toText gcode
  writeFileText fileName codeStr

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
