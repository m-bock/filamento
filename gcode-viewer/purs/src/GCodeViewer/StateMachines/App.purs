module GCodeViewer.StateMachines.App
  ( PubState
  , Msg
  , Dispatchers
  , tsApi
  , tsExports
  ) where

import GCodeViewer.Prelude

import Control.Monad.Error.Class (try)
import DTS as DTS
import Data.Argonaut.Core (Json)
import Data.Codec (encode)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Lens (set)
import Data.Newtype (class Newtype)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import GCodeViewer.Api (IndexFile, codecIndexFile)
import GCodeViewer.Api as Api
import GCodeViewer.Error (Err, printErr)
import GCodeViewer.RemoteData (RemoteData(..), codecRemoteData)
import GCodeViewer.TsBridge (class TsBridge, Tok(..))
import Stadium.Core (DispatcherApi, TsApi, mkTsApi)
import TsBridge as TSB

type PubState =
  { index :: RemoteData IndexFile
  }

initPubState :: PubState
initPubState =
  { index: NotAsked
  }

data Msg = MsgSetIndex (RemoteData IndexFile)

updatePubState :: Msg -> PubState -> Except String PubState
updatePubState msg pubState = case msg of
  MsgSetIndex r -> pubState # set (prop @"index") r # pure

encodeMsg :: Msg -> { tag :: String, args :: Json }
encodeMsg = case _ of
  MsgSetIndex r ->
    { tag: "MsgSetIndex"
    , args: CA.encode (codecRemoteData codecIndexFile) r
    }

type Dispatchers =
  { msg :: EffectFn1 Msg Unit
  , runFetchIndex :: EffectFn1 { url :: String } Unit
  }

dispatchers :: DispatcherApi Msg PubState {} -> Dispatchers
dispatchers { emitMsg, emitMsgCtx, readPubState } =
  { msg: mkEffectFn1 emitMsg
  , runFetchIndex: run fetchIndex
  }
  where
  fetchIndex :: { url :: String } -> ExceptT Err Aff Unit
  fetchIndex { url } = do
    index <- Api.getIndexFile { url }
    pure unit

run :: forall a. (a -> ExceptT Err Aff Unit) -> EffectFn1 a Unit
run f = mkEffectFn1 \arg -> launchAff_ do
  result <- runExceptT $ f arg
  case result of
    Left err -> log (printErr err)
    Right _ -> pure unit

tsApi :: TsApi Msg PubState {} Dispatchers
tsApi = mkTsApi
  { updatePubState: \msg s -> runExcept (updatePubState msg s)
  , dispatchers
  , initPubState
  , initPrivState: {}
  , printError: identity
  , encodeJsonPubState: encode codecPubState
  , encodeMsg
  }

codecPubState :: JsonCodec PubState
codecPubState = CAR.object "PubState"
  { index: codecRemoteData codecIndexFile
  }

newtype PubStateAlias = PubStateAlias PubState

derive instance Newtype (PubStateAlias) _

instance TsBridge (PubStateAlias) where
  tsBridge = TSB.tsBridgeNewtype0 Tok { moduleName, typeName: "PubState" }

newtype DispatchersAlias = DispatchersAlias Dispatchers

derive instance Newtype (DispatchersAlias) _

instance TsBridge (DispatchersAlias) where
  tsBridge = TSB.tsBridgeNewtype0 Tok { moduleName, typeName: "Dispatchers" }

instance TsBridge Msg where
  tsBridge = TSB.tsBridgeOpaqueType { moduleName, typeName: "Msg", typeArgs: [] }

-----

moduleName :: String
moduleName = "GCodeViewer.StateMachines.App"

tsExports :: Either TSB.AppError (Array DTS.TsModuleFile)
tsExports = TSB.tsModuleFile moduleName
  [ TSB.tsValues Tok
      { -- tsApi: coerce tsApi :: TsApi Msg PubStateAlias {} DispatchersAlias
      }
  ]