{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Octo where

import Control.Concurrent (threadDelay)
import Control.Lens ((^?))
import Data.Aeson
import Data.Aeson.Lens (key, _String)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client.MultipartFormData (Part, partBS, partFileSource)
import Network.HTTP.Req
import Relude
import qualified Sketch02
import qualified Sketch03

-- main :: IO ()
-- main = do
--   -- Sketch01.main
--   Sketch03.main

-- Config
host :: Url 'Http
host = http "localhost" /: "api"

octoPort :: Option 'Http
octoPort = port 5000

apiKey :: Text
apiKey = "D4FAeB-6AHb3aZ3bslkvwmvMDcF3QUkQvfG4MLQyJ8o"

headers :: Option scheme
headers =
  header "X-Api-Key" (encodeUtf8 apiKey)
    <> header "Content-Type" "application/json"

-- Wait until OctoPrint is ready
waitForServer :: IO ()
waitForServer = runReq defaultHttpConfig $ do
  liftIO $ putStrLn "Waiting for OctoPrint..."
  let try = do
        r <- req GET (host /: "version") NoReqBody jsonResponse (headers <> octoPort) :: Req (JsonResponse Value)
        let ok = responseStatusCode r == 200
        if ok
          then liftIO $ putStrLn "OctoPrint is ready."
          else retry
      retry = liftIO (threadDelay 1000000) >> try
  try

connectPrinter :: IO ()
connectPrinter =
  runReq defaultHttpConfig $
    do
      putStrLn "Connecting to printer..."

      let req1 = do
            _ <-
              req
                POST
                (host /: "connection")
                (ReqBodyJson $ object ["command" .= ("connect" :: Text)])
                ignoreResponse
                (headers <> octoPort)

            req2 0

          req2 10 = do
            putStrLn "Failed to connect after 10 attempts."
            req1
          req2 n = do
            r <- req GET (host /: "connection") NoReqBody jsonResponse (headers <> octoPort) :: Req (JsonResponse Value)
            let state = responseBody r ^? key "current" . key "state" . _String
            case state of
              Just "Operational" -> putStrLn "Printer connected."
              _ -> do
                putStrLn ("Printer not connected yet, waiting... " <> "state: " <> show state)
                liftIO $ threadDelay 1000000
                req2 (n + 1)

      req1

-- Send G-code
sendGCode :: [Text] -> IO ()
sendGCode cmds = runReq defaultHttpConfig $ do
  liftIO $ putStrLn $ "Sending G-code: " <> T.unpack (T.intercalate ", " cmds)
  _ <-
    req
      POST
      (host /: "printer" /: "command")
      (ReqBodyJson $ object ["commands" .= cmds])
      ignoreResponse
      (headers <> octoPort)
  pure ()

printGCodeFile :: FilePath -> IO ()
printGCodeFile filePath = runReq defaultHttpConfig $ do
  liftIO $ putStrLn $ "Uploading file: " <> filePath

  -- Assemble multipart body using low-level http-client API
  body <-
    reqBodyMultipart
      [ partFileSource "file" filePath,
        partBS "select" "true",
        partBS "print" "true"
      ]

  -- Perform request
  r <-
    req
      POST
      (http "localhost" /: "api" /: "files" /: "local")
      body
      jsonResponse
      (header "X-Api-Key" (encodeUtf8 apiKey) <> port 5000)

  liftIO $ print (responseBody r :: Value)

main :: IO ()
main = do
  waitForServer
  connectPrinter
  printGCodeFile "out/myprint.gcode"