module GCodeViewer.State where

import Prelude

import DTS as DTS
import Data.Either (Either)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Fiber, delay, launchAff_)
import Effect.Class (liftEffect)
import GCodeViewer.Lib (MkAppState, DispatcherApi, TsApi, tsApiToDispatcherApi)
import GCodeViewer.Lib as Lib
import GCodeViewer.TsBridge (class TsBridge, Tok(..))
import TsBridge (tsTypeAlias)
import TsBridge as TSB
import Type.Prelude (Proxy(..))

-- Public state

type AppState = Lib.MkAppState PubState PrivState

type PubState =
  { startLayer :: Int
  , endLayer :: Int
  , indexFile :: String
  }

initPubState :: PubState
initPubState =
  { startLayer: 0
  , endLayer: 0
  , indexFile: ""
  }

data Msg
  = MsgChangeStartLayer Int
  | MsgChangeEndLayer Int
  | MsgChangeIndexFile String

updatePubState :: Msg -> PubState -> PubState
updatePubState msg pubState = case msg of
  MsgChangeStartLayer startLayer -> pubState { startLayer = startLayer }
  MsgChangeEndLayer endLayer -> pubState { endLayer = endLayer }
  MsgChangeIndexFile indexFile -> pubState { indexFile = indexFile }

-- Priv state

newtype PrivState = PrivState
  { fetchIndexFiber :: Maybe (Fiber Unit)
  }

initPrivState :: PrivState
initPrivState = PrivState
  { fetchIndexFiber: Nothing
  }

-- State

initialState :: AppState
initialState = Lib.MkAppState
  { pubState: initPubState
  , privState: initPrivState
  }

---

type Dispatchers =
  { fetchIndexFile :: Effect Unit
  , changeStartLayer :: Int -> Effect Unit
  , changeEndLayer :: Int -> Effect Unit
  }

dispatchers :: DispatcherApi Msg PubState PrivState -> Dispatchers
dispatchers { emitMsg } =
  { fetchIndexFile: launchAff_ do
      delay (Milliseconds 3000.0)
      let content = "content of index.json"
      liftEffect $ emitMsg (MsgChangeIndexFile content)
  , changeStartLayer: \n -> (emitMsg (MsgChangeStartLayer n))
  , changeEndLayer: \n -> emitMsg (MsgChangeEndLayer n)
  }

---

tsDispatchers :: TsApi AppState -> Dispatchers
tsDispatchers tsApi = dispatchers (tsApiToDispatcherApi updatePubState tsApi)

intToNumber :: Int -> Number
intToNumber = Int.toNumber

round :: Number -> Int
round = Int.round

getPubState :: AppState -> PubState
getPubState (Lib.MkAppState state) = state.pubState

---

eqPubState :: PubState -> PubState -> Boolean
eqPubState a b = a == b

moduleName :: String
moduleName = "GCodeViewer.State"

instance TsBridge Msg where
  tsBridge = TSB.tsBridgeOpaqueType
    { moduleName
    , typeName: "Msg"
    , typeArgs: []
    }

-- --

instance TsBridge PrivState where
  tsBridge = TSB.tsBridgeOpaqueType
    { moduleName
    , typeName: "PrivState"
    , typeArgs: []
    }

tsExports :: Either TSB.AppError (Array DTS.TsModuleFile)
tsExports = TSB.tsModuleFile moduleName
  [ TSB.tsValues Tok
      { initialState
      , tsDispatchers
      , getPubState
      , eqPubState
      , intToNumber
      , round
      }
  , tsTypeAlias Tok "AppState" (Proxy :: _ AppState)
  , tsTypeAlias Tok "PubState" (Proxy :: _ PubState)
  , tsTypeAlias Tok "Dispatchers" (Proxy :: _ Dispatchers)
  ]