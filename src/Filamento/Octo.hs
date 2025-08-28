{-# LANGUAGE NumericUnderscores #-}

module Filamento.Octo where

import Control.Concurrent (threadDelay)
import Marlin.Core (GCodeLine)
import Octo.API
import Relude

sendGCode :: OctoHttpCfg -> Text -> IO ()
sendGCode cfg gcode = do
  sendText cfg gcode

  let go = do
        threadDelay 1_000_000 -- 1s
        jobState <- getJobState cfg
        case jobState of
          OctoStateOperational -> do
            pure ()
          _ -> do
            go

  go

getJobState :: OctoHttpCfg -> IO OctoState
getJobState cfg = do
  job <- getApiConnection cfg
  pure job.current.state

sendText :: OctoHttpCfg -> Text -> IO ()
sendText cfg text = do
  let filePath = "out/current.gcode"
  writeFileText (toString filePath) text
  postApiFilesLocal
    cfg
    RequestPostApiFilesLocal
      { select = True,
        print = True,
        filePath
      }