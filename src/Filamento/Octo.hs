{-# LANGUAGE NumericUnderscores #-}

module Filamento.Octo where

import Control.Concurrent (threadDelay)
import Marlin.Core (GCodeLine)
import Octo.API
import Relude

sendGCode :: OctoHttpCfg -> [GCodeLine] -> IO ()
sendGCode cfg gcode = do
  sendText cfg (toText gcode)

  let go = do
        threadDelay 1_000_000 -- 1s
        jobState <- getJobState cfg
        case jobState of
          OctoStateOperational -> do
            pure ()
          OctoStateClosed -> do
            go

  go

getJobState :: OctoHttpCfg -> IO OctoState
getJobState cfg = do
  job <- getApiJob cfg
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