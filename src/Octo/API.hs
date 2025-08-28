module Octo.API
  ( postApiFilesLocal,
    getApiJob,
    OctoHttpCfg (..),
    RequestPostApiFilesLocal (..),
    ResponseGetApiJob (..),
    JobCurrent (..),
    OctoState (..),
  )
where

import Autodocodec (HasCodec, (.=))
import qualified Autodocodec as AD
import Autodocodec.DerivingVia (Autodocodec)
import Control.Exception (throwIO)
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import GHC.IO (catchException)
import GHC.IO.Exception (userError)
import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Types (Header)
import Network.HTTP.Types.Status
import Network.URI (URI)
import Relude

data OctoHttpCfg = OctoHttpCfg
  { manager :: Manager,
    apiKey :: Text,
    baseUrl :: URI
  }

data ErrCodeOcto
  = ErrOcto1
  | ErrOcto2
  | ErrOcto3
  | ErrOcto4
  | ErrOcto6
  deriving (Show, Eq, Typeable)

data ErrOcto = ErrOcto
  { errCode :: ErrCodeOcto,
    err :: SomeException
  }
  deriving (Show, Typeable)

instance Exception ErrOcto

mkOctoError :: ErrCodeOcto -> IO a -> IO a
mkOctoError errCode act = catchException act (\e -> throwIO (ErrOcto errCode e))

data RequestPostApiFilesLocal = RequestPostApiFilesLocal
  { select :: Bool,
    print :: Bool,
    filePath :: Text
  }
  deriving (Show, Eq)

mkCommonHeaders :: OctoHttpCfg -> [Header]
mkCommonHeaders cfg = [("X-Api-Key", encodeUtf8 cfg.apiKey)]

postApiFilesLocal :: OctoHttpCfg -> RequestPostApiFilesLocal -> IO ()
postApiFilesLocal cfg req = do
  initialRequest <- mkOctoError ErrOcto1 (requestFromURI cfg.baseUrl)

  let request =
        initialRequest
          { method = "POST",
            path = "/api/files/local",
            requestHeaders = mkCommonHeaders cfg
          }

  let printBool :: Bool -> ByteString
      printBool = \case
        True -> "true"
        False -> "false"

  formData <-
    mkOctoError ErrOcto2
      $ formDataBody
        [ partBS "select" (printBool req.select),
          partBS "print" (printBool req.print),
          partFileSource "file" (toString req.filePath)
        ]
        request

  response <- mkOctoError ErrOcto3 $ httpLbs formData cfg.manager

  when (response.responseStatus.statusCode /= 201) $ do
    mkOctoError ErrOcto4 do
      throwIO (userError $ show (response.responseStatus, response.responseBody))

data ResponseGetApiJob = ResponseGetApiJob
  { current :: JobCurrent
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec ResponseGetApiJob)

instance HasCodec ResponseGetApiJob where
  codec =
    AD.object "StatusResponse"
      $ ResponseGetApiJob
      <$> (AD.requiredField' "current" .= (\x -> x.current))

data JobCurrent = JobCurrent
  { state :: OctoState
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec JobCurrent)

instance HasCodec JobCurrent where
  codec =
    AD.object "JobCurrent"
      $ JobCurrent
      <$> (AD.requiredField' "state" .= (\x -> x.state))

data OctoState
  = OctoStateOperational
  | OctoStateClosed
  deriving (Show, Eq, Enum, Bounded)
  deriving (FromJSON, ToJSON) via (Autodocodec OctoState)

instance HasCodec OctoState where
  codec = AD.boundedEnumCodec $ \case
    OctoStateOperational -> "Operational"
    OctoStateClosed -> "Closed"

getApiJob :: OctoHttpCfg -> IO ResponseGetApiJob
getApiJob cfg = do
  initialRequest <- mkOctoError ErrOcto1 (requestFromURI cfg.baseUrl)
  let request =
        initialRequest
          { method = "GET",
            path = "/api/job",
            requestHeaders = mkCommonHeaders cfg
          }
  response <- mkOctoError ErrOcto3 $ httpLbs request cfg.manager

  when (response.responseStatus.statusCode /= 200) $ do
    mkOctoError ErrOcto4 do
      throwIO (userError $ show (response.responseStatus, response.responseBody))

  res <- mkOctoError ErrOcto6 do
    either (throwIO . userError) pure (eitherDecode response.responseBody)

  pure res
