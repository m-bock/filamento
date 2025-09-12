module GCodeViewer.RemoteData where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import DTS as DTS
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Codec (decode)
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError(..))
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import GCodeViewer.CodecExtra as CE
import GCodeViewer.Prelude (class Generic, Either)
import GCodeViewer.TsBridge (class TsBridge, Tok(..))
import Stadium.TL (mkConstructors, mkMatcher)
import TsBridge (TypeVar)
import TsBridge as TSB
import Unsafe.Coerce (unsafeCoerce)

type RemoteData a =
  { value :: a
  , status :: RemoteDataStatus
  }

codecRemoteData :: forall a. JsonCodec a -> JsonCodec (RemoteData a)
codecRemoteData codecValue = CAR.object "RemoteData"
  { value: codecValue
  , status: codecRemoteDataStatus
  }

codecRemoteDataStatus :: JsonCodec RemoteDataStatus
codecRemoteDataStatus = CA.codec' dec enc
  where
  enc :: RemoteDataStatus -> Json
  enc = case _ of
    NotAsked -> CE.encTag "NotAsked"
    Loading -> CE.encTag "Loading"
    Loaded -> CE.encTag "Loaded"
    Error r -> CE.encTagWithArgs "Error" (CAR.object "Error" { message: CA.string }) r

  dec :: Json -> Either JsonDecodeError RemoteDataStatus
  dec j =
    CE.decTag "NotAsked" NotAsked j
      <|> CE.decTag "Loading" Loading j
      <|> CE.decTag "Loaded" Loaded j
      <|> CE.decTagWithArgs "Error" Error (CAR.object "Error" { message: CA.string }) j

data RemoteDataStatus
  = NotAsked
  | Loading
  | Loaded
  | Error { message :: String }

derive instance Eq RemoteDataStatus

derive instance Generic RemoteDataStatus _

mkRemoteDataStatus
  :: { "NotAsked" :: RemoteDataStatus
     , "Loading" :: RemoteDataStatus
     , "Loaded" :: RemoteDataStatus
     , "Error" :: { message :: String } -> RemoteDataStatus
     }
mkRemoteDataStatus = mkConstructors @RemoteDataStatus

onRemoteDataStatus
  :: forall @z
   . { notAsked :: z
     , loading :: z
     , loaded :: z
     , error :: { message :: String } -> z
     }
  -> RemoteDataStatus
  -> z
onRemoteDataStatus = \{ notAsked, loading, loaded, error } rd -> case rd of
  NotAsked -> notAsked
  Loading -> loading
  Loaded -> loaded
  Error err -> error err

---

moduleName :: String
moduleName = "GCodeViewer.RemoteData"

instance TsBridge RemoteDataStatus where
  tsBridge = TSB.tsBridgeOpaqueType
    { moduleName
    , typeName: "RemoteDataStatus"
    , typeArgs: []
    }

tsExports :: Either TSB.AppError (Array DTS.TsModuleFile)
tsExports = TSB.tsModuleFile moduleName
  [ TSB.tsValues Tok
      { mkRemoteDataStatus
      , onRemoteDataStatus: onRemoteDataStatus @(TypeVar "Z")
      }
  ]

---

data D = Foo Int -- | Bar | Baz

derive instance Generic D _

--onD :: forall a. { "Foo" :: a } -> D -> a
onD = mkMatcher @D :: ?A