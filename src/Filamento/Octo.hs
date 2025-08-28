module Filamento.Octo where

-- import Control.Concurrent (threadDelay)
-- import Data.Aeson
-- import Data.ByteString (findIndex)
-- import qualified Data.ByteString as BS
-- import qualified Data.Map.Strict as Map
-- import Data.Text (Text)
-- import qualified Data.Text as T
-- import Filamento
-- import Filamento.Filament
-- import Filamento.Math
-- import GHC.Conc
-- import Linear
-- import Network.HTTP.Client
-- import Network.HTTP.Client.MultipartFormData
-- import qualified Network.HTTP.Req as Req
-- import Network.HTTP.Types.Status
-- import Relude
-- import System.Environment (setEnv)
-- import qualified System.IO as IO

-- sendFile :: Text -> Text -> IO ()
-- sendFile apiKey filePath = do
--   liftIO $ putStrLn $ "Sending file: " <> T.unpack filePath

--   -- Check if file exists first
--   exists <- doesFileExist (toString filePath)
--   if exists
--     then do
--       -- Read file content as ByteString for upload
--       fileContent <- readFileBS (toString filePath)
--       putStrLn $ "File exists, size: " <> show (BS.length fileContent) <> " bytes"

--       -- Upload file to OctoPrint using http-client for multipart support
--       liftIO $ putStrLn "Uploading file to OctoPrint..."

--       -- Create HTTP client manager
--       manager <- newManager defaultManagerSettings

--       -- Create the request
--       let url = "http://localhost:5000/api/files/local"
--       initialRequest <- parseRequest url
--       let request =
--             initialRequest
--               { method = "POST",
--                 requestHeaders = [("X-Api-Key", encodeUtf8 apiKey)]
--               }

--       -- Create multipart form data
--       formData <-
--         formDataBody
--           [ partBS "select" "true",
--             partBS "print" "true",
--             partFileSource "file" (toString filePath)
--           ]
--           request

--       -- Send the request
--       response <- httpLbs formData manager

--       liftIO $ putStrLn $ "Upload response status: " <> show (statusCode $ responseStatus response)
--       liftIO $ putStrLn $ "Response body: " <> show (responseBody response)
--     else
--       putStrLn "File does not exist"

-- sendGCode :: Text -> [Text] -> IO ()
-- sendGCode apiKey cmds = Req.runReq Req.defaultHttpConfig $ do
--   liftIO $ putStrLn $ "Sending G-code: " <> T.unpack (T.intercalate ", " cmds)

--   -- For now, let's try without CSRF token first
--   -- OctoPrint might accept the request with just the API key
--   res :: Req.JsonResponse Text <-
--     Req.req
--       Req.POST
--       (Req.http "localhost" Req./: "api" Req./: "printer" Req./: "command")
--       (Req.ReqBodyJson $ object ["commands" .= cmds])
--       Req.jsonResponse
--       (Req.header "X-Api-Key" (encodeUtf8 apiKey) <> Req.port 5000)

--   liftIO $ print (Req.responseBody res)

--   pure ()
