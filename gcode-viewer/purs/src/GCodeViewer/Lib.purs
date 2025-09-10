module GCodeViewer.Lib
  ( FullState
  , mkTsApi
  , DispatcherApi
  , TsApi(..)
  , TsStateHandle
  , logJson
  , tsExports
  ) where

import GCodeViewer.Prelude

import DTS as DTS
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Lens (over, set)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import GCodeViewer.TsBridge (class TsBridge, Tok(..), tsBridge)
import TsBridge (TypeVar)
import TsBridge as TSB
import Type.Prelude (Proxy(..))
import Web.DOM.Document (doctype)

-- Public state

type FullState msg pubState privState =
  { pubState :: pubState
  , privState :: privState
  , history :: Array { msg :: msg, pubState :: pubState }
  , historyIndex :: Number
  }

type DispatcherApi msg pubState privState =
  { emitMsg :: msg -> Effect Unit
  , readPubState :: Effect pubState
  , readPrivState :: Effect privState
  , updatePrivState :: (privState -> privState) -> Effect Unit
  }

---

type PursConfig msg pubState privState err disp =
  { updatePubState :: msg -> pubState -> Either String pubState
  , dispatchers :: DispatcherApi msg pubState privState -> disp
  , printError :: err -> String
  , initPubState :: pubState
  , initPrivState :: privState
  , encodeJsonPubState :: pubState -> Json
  , encodeJsonMsg :: msg -> Json
  }

type TsStateHandle state =
  { updateState :: (state -> Effect state) -> Effect Unit
  , readState :: Effect state
  }

newtype TsApi msg pubState privState disp = TsApi
  { dispatchers :: TsStateHandle (FullState msg pubState privState) -> disp
  , initState :: FullState msg pubState privState
  , timeTravel :: EffectFn1 Number Unit
  }

derive instance Newtype (TsApi msg pubState state disp) _

instance (TsBridge pubState, TsBridge privState, TsBridge disp, TsBridge msg) => TsBridge (TsApi msg pubState privState disp) where
  tsBridge = TSB.tsBridgeNewtype Tok
    { moduleName
    , typeName: "TsApi"
    , typeArgs:
        [ "msg" /\ tsBridge (Proxy :: _ msg)
        , "pubState" /\ tsBridge (Proxy :: _ pubState)
        , "privState" /\ tsBridge (Proxy :: _ privState)
        , "disp" /\ tsBridge (Proxy :: _ disp)
        ]
    }

mkTsApi :: forall msg pubState privState err disp. PursConfig msg pubState privState err disp -> TsApi msg pubState privState disp
mkTsApi { initPubState, initPrivState, dispatchers, updatePubState, encodeJsonPubState, encodeJsonMsg } =
  TsApi
    { dispatchers: f >>> dispatchers
    , initState: { history: [], historyIndex: 0.0, pubState: initPubState, privState: initPrivState }
    , timeTravel: mkEffectFn1 \n -> pure unit
    }
  where
  f :: TsStateHandle (FullState msg pubState privState) -> DispatcherApi msg pubState privState
  f ts =
    { emitMsg: \msg -> ts.updateState
        ( \state -> case updatePubState msg state.pubState of
            Left err -> do
              log err
              pure state
            Right newState -> do
              logJson
                [ encodeJson "msg"
                , encodeJsonMsg msg
                ]

              logJson
                [ encodeJson "newState"
                , encodeJsonPubState newState
                ]

              pure
                ( state
                    # set (prop @"pubState") newState
                    # set (prop @"historyIndex") (state.historyIndex + 1.0)
                    # over (prop @"history") (\xs -> xs <> [ { msg, pubState: newState } ])
                )
        )
    , readPubState: do
        st <- ts.readState
        pure st.pubState
    , readPrivState: do
        st <- ts.readState
        pure st.privState
    , updatePrivState: \f ->
        ts.updateState (\(state) -> pure (state { privState = f state.privState }))
    }

---

moduleName :: String
moduleName = "GCodeViewer.Lib"

tsExports :: Either TSB.AppError (Array DTS.TsModuleFile)
tsExports = TSB.tsModuleFile moduleName
  [ TSB.tsValues Tok
      {
      }
  --, TSB.tsTypeAlias Tok "TsApi" (Proxy :: _ (TsApi (TypeVar "A") (TypeVar "B") (TypeVar "C")))
  ]

foreign import logJson :: Array Json -> Effect Unit