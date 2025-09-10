module GCodeViewer.StateMachine where

import GCodeViewer.Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (catchError)
import DTS as DTS
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Codec (encode)
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Lens (set)
import Data.Newtype (class Newtype)
import Data.String as Str
import Effect.Aff (launchAff)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import GCodeViewer.Api as Api
import GCodeViewer.CodecExtra as CE
import GCodeViewer.Error (Err, mkErr, printErr)
import GCodeViewer.Error as Err
import GCodeViewer.Lib (DispatcherApi, TsApi, logJson, mkTsApi)
import GCodeViewer.RemoteData (RemoteData, RemoteDataStatus(..), codecRemoteData, codecRemoteDataStatus)
import GCodeViewer.TsBridge (class TsBridge, Tok(..))
import Safe.Coerce (coerce)
import TsBridge as TSB
import Unsafe.Coerce (unsafeCoerce)

type PubState =
  { gcodeLines :: RemoteData (Array String)
  , startLayer :: Int
  , endLayer :: Int
  }

initPubState :: PubState
initPubState =
  { gcodeLines: { value: [], status: NotAsked }
  , startLayer: 0
  , endLayer: 0
  }

updatePubState :: Msg -> PubState -> Except String PubState
updatePubState msg pubState = case msg of
  MsgSetStartLayer startLayer -> pubState
    # set (prop @"startLayer") startLayer.layer
    # pure

  MsgSetEndLayer endLayer -> pubState
    # set (prop @"endLayer") endLayer.layer
    # pure

  MsgSetGcodeLines { value, status } -> pubState
    # set (prop @"gcodeLines" <<< prop @"status") status
    # case value of
        Just value -> set (prop @"gcodeLines" <<< prop @"value") value
        Nothing -> identity
    # pure

data Msg
  = MsgSetStartLayer { layer :: Int }
  | MsgSetEndLayer { layer :: Int }
  | MsgSetGcodeLines { value :: Maybe (Array String), status :: RemoteDataStatus }

runSafe :: ExceptT Err Aff Unit -> Effect Unit
runSafe act = launchAff_ do
  result <- runExceptT act
  case result of
    Left err -> log (printErr err)
    Right _ -> pure unit

type Dispatchers =
  { setStartLayer :: { layer :: Int } -> Effect Unit
  , setEndLayer :: { layer :: Int } -> Effect Unit
  , loadGcodeLines :: EffectFn1 { url :: String } Unit
  }

run :: forall a. (a -> ExceptT Err Aff Unit) -> EffectFn1 a Unit
run f = mkEffectFn1 \arg -> launchAff_ do
  result <- runExceptT $ f arg
  case result of
    Left err -> log (printErr err)
    Right _ -> pure unit

dispatchers :: DispatcherApi Msg PubState {} -> Dispatchers
dispatchers { emitMsg, readPubState } =
  { setStartLayer: emitMsg <<< MsgSetStartLayer
  , setEndLayer: emitMsg <<< MsgSetEndLayer
  , loadGcodeLines: run loadGcodeLines
  }
  where
  loadGcodeLines :: { url :: String } -> ExceptT Err Aff Unit
  loadGcodeLines { url } = flip catchError
    ( \e ->
        --liftEffect $ emitMsg $ MsgSetGcodeLines { value: Nothing, status: Error { message: Err.printErr e } }
        log $ Err.printErr e
    )
    do
      log "loadGcodeLines"

      st <- liftEffect $ readPubState

      when (st.gcodeLines.status == Loading) $ do
        throwError (mkErr Err.ErrX "Gcode lines are already loading")

      liftEffect $ emitMsg $ MsgSetGcodeLines { value: Nothing, status: Loading }

      ret <- Api.getGCodeFile url

      liftAff $ delay (Milliseconds 5000.0)

      let lines = Str.split (Str.Pattern "\n") ret

      liftEffect $ emitMsg $ MsgSetGcodeLines { value: Just lines, status: Loaded }

tsApi :: TsApi Msg PubState {} Dispatchers
tsApi = mkTsApi
  { updatePubState: \msg s -> runExcept (updatePubState msg s)
  , dispatchers
  , initPubState
  , initPrivState: {}
  , printError: identity
  , encodeJsonPubState: encode codecPubState
  , encodeJsonMsg: encode codecMsg
  }

codecMsg :: JsonCodec Msg
codecMsg = CA.codec' dec enc
  where
  dec :: Json -> Either JsonDecodeError Msg
  dec j = CE.decTagWithArgs "MsgSetStartLayer" MsgSetStartLayer (CAR.object "MsgSetStartLayer" { layer: CA.int }) j
    <|> CE.decTagWithArgs "MsgSetEndLayer" MsgSetEndLayer (CAR.object "MsgSetEndLayer" { layer: CA.int }) j
    <|> CE.decTagWithArgs "MsgSetGcodeLines" MsgSetGcodeLines (CAR.object "MsgSetGcodeLines" { value: CAC.maybe (CA.array CA.string), status: codecRemoteDataStatus }) j

  enc :: Msg -> Json
  enc = case _ of
    MsgSetStartLayer startLayer ->
      CE.encTagWithArgs "MsgSetStartLayer" (CAR.object "MsgSetStartLayer" { layer: CA.int }) startLayer
    MsgSetEndLayer endLayer ->
      CE.encTagWithArgs "MsgSetEndLayer" (CAR.object "MsgSetEndLayer" { layer: CA.int }) endLayer
    MsgSetGcodeLines r ->
      CE.encTagWithArgs "MsgSetGcodeLines" (CAR.object "MsgSetGcodeLines" { value: CAC.maybe (CA.array CA.string), status: codecRemoteDataStatus }) r

codecPubState :: JsonCodec PubState
codecPubState = CAR.object "PubState"
  { gcodeLines: codecRemoteData (CA.array CA.string)
  , startLayer: CA.int
  , endLayer: CA.int
  }

newtype PubStateAlias = PubStateAlias PubState

derive instance Newtype (PubStateAlias) _

instance TsBridge (PubStateAlias) where
  tsBridge = TSB.tsBridgeNewtype Tok
    { moduleName
    , typeName: "PubState"
    , typeArgs: []
    }

newtype DispatchersAlias = DispatchersAlias Dispatchers

derive instance Newtype (DispatchersAlias) _

instance TsBridge (DispatchersAlias) where
  tsBridge = TSB.tsBridgeNewtype Tok
    { moduleName
    , typeName: "Dispatchers"
    , typeArgs: []
    }

instance TsBridge Msg where
  tsBridge = TSB.tsBridgeOpaqueType
    { moduleName
    , typeName: "Msg"
    , typeArgs: []
    }

-----

moduleName :: String
moduleName = "GCodeViewer.StateMachine"

tsExports :: Either TSB.AppError (Array DTS.TsModuleFile)
tsExports = TSB.tsModuleFile moduleName
  [ TSB.tsValues Tok
      { tsApi: coerce tsApi :: TsApi Msg PubStateAlias {} DispatchersAlias
      }
  ]