module GCodeViewer.RemoteData where

import Prelude

import DTS as DTS
import GCodeViewer.Prelude (Either)
import GCodeViewer.TsBridge (class TsBridge, Tok(..))
import TsBridge (TypeVar)
import TsBridge as TSB

type RemoteData a =
  { value :: a
  , status :: RemoteDataStatus
  }

data RemoteDataStatus
  = NotAsked
  | Loading
  | Loaded
  | Error { message :: String }

derive instance Eq RemoteDataStatus

mkRemoteDataStatus
  :: { notAsked :: RemoteDataStatus
     , loading :: RemoteDataStatus
     , loaded :: RemoteDataStatus
     , error :: { message :: String } -> RemoteDataStatus
     }
mkRemoteDataStatus =
  { notAsked: NotAsked
  , loading: Loading
  , loaded: Loaded
  , error: Error
  }

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