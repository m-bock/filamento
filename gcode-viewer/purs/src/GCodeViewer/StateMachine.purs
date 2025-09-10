module GCodeViewer.StateMachine where

import GCodeViewer.Prelude

import Control.Monad.Error.Class (catchError)
import DTS as DTS
import Data.Lens (set)
import Data.Lens.Iso.Newtype (unto)
import Data.Newtype (class Newtype)
import Data.String as Str
import GCodeViewer.Api as Api
import GCodeViewer.Error (Err, mkErr, printErr)
import GCodeViewer.Error as Err
import GCodeViewer.Lib (DispatcherApi, MkAppState, TsApi, mkTsApi)
import GCodeViewer.RemoteData (RemoteData, RemoteDataStatus(..))
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
    # set (prop @"startLayer") startLayer
    # pure

  MsgSetEndLayer endLayer -> pubState
    # set (prop @"endLayer") endLayer
    # pure

  MsgSetGcodeLines { value, status } -> pubState
    # set (prop @"gcodeLines" <<< prop @"status") status
    # case value of
        Just value -> set (prop @"gcodeLines" <<< prop @"value") value
        Nothing -> identity
    # pure

data Msg
  = MsgSetStartLayer Int
  | MsgSetEndLayer Int
  | MsgSetGcodeLines { value :: Maybe (Array String), status :: RemoteDataStatus }

runSafe :: ExceptT Err Aff Unit -> Effect Unit
runSafe act = launchAff_ do
  result <- runExceptT act
  case result of
    Left err -> log (printErr err)
    Right _ -> pure unit

type Dispatchers =
  { setStartLayer :: Int -> Effect Unit
  , setEndLayer :: Int -> Effect Unit
  , loadGcodeLines :: { url :: String } -> Effect Unit
  }

dispatchers :: DispatcherApi Msg PubState {} -> Dispatchers
dispatchers { emitMsg, readPubState } =
  { setStartLayer: emitMsg <<< MsgSetStartLayer
  , setEndLayer: emitMsg <<< MsgSetEndLayer
  , loadGcodeLines: runSafe <<< loadGcodeLines
  }
  where
  loadGcodeLines :: { url :: String } -> ExceptT Err Aff Unit
  loadGcodeLines { url } = flip catchError
    ( \e ->
        liftEffect $ emitMsg $ MsgSetGcodeLines { value: Nothing, status: Error { message: Err.printErr e } }
    )
    do
      st <- liftEffect $ readPubState

      when (st.gcodeLines.status == Loading) $ do
        throwError (mkErr Err.ErrX "Gcode lines are already loading")

      liftEffect $ emitMsg $ MsgSetGcodeLines { value: Nothing, status: Loading }

      ret <- Api.getGCodeFile url

      let lines = Str.split (Str.Pattern "\n") ret

      liftEffect $ emitMsg $ MsgSetGcodeLines { value: Just lines, status: Loaded }

tsApi :: TsApi PubState {} Dispatchers
tsApi = mkTsApi
  { updatePubState: \msg s -> runExcept (updatePubState msg s)
  , dispatchers
  , initPubState
  , initPrivState: {}
  , printError: identity
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

-----

moduleName :: String
moduleName = "GCodeViewer.StateMachine"

tsExports :: Either TSB.AppError (Array DTS.TsModuleFile)
tsExports = TSB.tsModuleFile moduleName
  [ TSB.tsValues Tok
      { tsApi: coerce tsApi :: TsApi PubStateAlias {} DispatchersAlias
      }
  ]