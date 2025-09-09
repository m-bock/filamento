module GCodeViewer.Api where

import Prelude

import Affjax as Affjax
import Affjax.ResponseFormat (json, string)
import Affjax.StatusCode (StatusCode(..))
import Affjax.Web as AffjaxWeb
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Effect.Aff (Aff)
import GCodeViewer.Error (Err, handleAffEither, handleEither, mkErr)
import GCodeViewer.Error as Err

-------------------------------------------------------------------------------

type IndexFile =
  Array IndexFileItem

codecIndexFile :: JsonCodec IndexFile
codecIndexFile = CA.array codecIndexFileItem

-------------------------------------------------------------------------------

type IndexFileItem =
  { name :: String
  , gcode :: String
  , pictures :: Array String
  }

codecIndexFileItem :: JsonCodec IndexFileItem
codecIndexFileItem = CAR.object "IndexFileItem"
  { name: CA.string
  , gcode: CA.string
  , pictures: CA.array CA.string
  }

getIndexFile :: ExceptT Err Aff IndexFile
getIndexFile = do
  ret <- handleAffEither (mkErr Err.Err1 <<< Affjax.printError) $ AffjaxWeb.get json "/out/index.json"

  when (ret.status /= StatusCode 200) $ throwError (mkErr Err.Err3 "Failed to get index file")

  val <- handleEither (mkErr Err.Err2 <<< CA.printJsonDecodeError) $ CA.decode codecIndexFile ret.body

  pure val

-------------------------------------------------------------------------------

getGCodeFile :: String -> ExceptT Err Aff String
getGCodeFile url = do
  ret <- handleAffEither (mkErr Err.Err4 <<< Affjax.printError) $ AffjaxWeb.get string ("/out/" <> url)

  when (ret.status /= StatusCode 200) $ throwError (mkErr Err.Err5 "Failed to get gcode file")

  pure ret.body
