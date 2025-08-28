module Octo.API
  ( postApiFilesLocal,
    getApiConnection,
    OctoHttpCfg (..),
    RequestPostApiFilesLocal (..),
    ResponseGetApiConnection (..),
    JobCurrent (..),
    OctoState (..),
  )
where

import Autodocodec (HasCodec, (.=))
import qualified Autodocodec as AD
import Autodocodec.DerivingVia (Autodocodec)
import Control.Exception (throwIO)
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import qualified Data.ByteString.Lazy as LB
import Data.String.Conversions (cs)
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

data ResponseGetApiConnection = ResponseGetApiConnection
  { current :: JobCurrent
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec ResponseGetApiConnection)

instance HasCodec ResponseGetApiConnection where
  codec =
    AD.object "StatusResponse"
      $ ResponseGetApiConnection
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
  | OctoStatePrinting
  deriving (Show, Eq, Enum, Bounded)
  deriving (FromJSON, ToJSON) via (Autodocodec OctoState)

instance HasCodec OctoState where
  codec = AD.boundedEnumCodec $ \case
    OctoStateOperational -> "Operational"
    OctoStateClosed -> "Closed"
    OctoStatePrinting -> "Printing"

getApiConnection :: OctoHttpCfg -> IO ResponseGetApiConnection
getApiConnection cfg = do
  initialRequest <- mkOctoError ErrOcto1 (requestFromURI cfg.baseUrl)
  let request =
        initialRequest
          { method = "GET",
            path = "/api/connection",
            requestHeaders = mkCommonHeaders cfg
          }
  response <- mkOctoError ErrOcto3 $ httpLbs request cfg.manager

  when (response.responseStatus.statusCode /= 200) $ do
    mkOctoError ErrOcto4 do
      throwIO (userError $ show (response.responseStatus, response.responseBody))

  res <- mkOctoError ErrOcto6 do
    parseEitherIO response.responseBody

  pure res

parseEitherIO :: (FromJSON a) => LB.ByteString -> IO a
parseEitherIO str = case eitherDecode str of
  Left msg -> throwIO (userError $ cs $ cs msg <> "\n" <> str)
  Right x -> pure x