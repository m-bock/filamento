module GCodeViewer.Lib
  ( FullState(..)
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
import Data.Lens (over, set)
import Data.Maybe (maybe)
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import GCodeViewer.TagName (class TagName, tagName)
import GCodeViewer.TsBridge (class TsBridge, Tok(..), tsBridge)
import TsBridge as TSB
import Type.Prelude (Proxy(..))

-- Public state

newtype FullState msg pubState privState = FullState
  { pubState :: pubState
  , privState :: privState
  , history :: Array { msg :: msg, pubState :: pubState }
  , historyIndex :: Number
  }

type DispatcherApi msg pubState privState =
  { emitMsg :: msg -> Effect Unit
  , emitMsgCtx :: String -> msg -> Effect Unit
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

instance
  ( TsBridge pubState
  , TsBridge privState
  , TsBridge disp
  , TsBridge msg
  ) =>
  TsBridge (TsApi msg pubState privState disp) where
  tsBridge = TSB.tsBridgeNewtype4 @"msg" @"pubState" @"privState" @"disp" Tok { moduleName, typeName: "TsApi" }

derive instance Newtype (FullState msg pubState privState) _

instance
  ( TsBridge msg
  , TsBridge pubState
  , TsBridge privState
  ) =>
  TsBridge (FullState msg pubState privState) where
  tsBridge = TSB.tsBridgeNewtype2 @"msg" @"pubState" Tok
    { moduleName
    , typeName: "FullState"
    }

mkTsApi :: forall msg pubState privState err disp. TagName msg => PursConfig msg pubState privState err disp -> TsApi msg pubState privState disp
mkTsApi { initPubState, initPrivState, dispatchers, updatePubState, encodeJsonPubState } =
  TsApi
    { dispatchers: f >>> dispatchers
    , initState: FullState { history: [], historyIndex: 0.0, pubState: initPubState, privState: initPrivState }
    , timeTravel: mkEffectFn1 \n -> pure unit
    }
  where
  f :: TsStateHandle (FullState msg pubState privState) -> DispatcherApi msg pubState privState
  f ts =
    { emitMsg: emitMsg Nothing
    , emitMsgCtx: \ctx -> emitMsg (Just ctx)
    , readPubState: do
        FullState st <- ts.readState
        pure st.pubState
    , readPrivState: do
        FullState st <- ts.readState
        pure st.privState
    , updatePrivState: \f ->
        ts.updateState (\(FullState state) -> pure (FullState state { privState = f state.privState }))
    }
    where
    emitMsg :: Maybe String -> msg -> Effect Unit
    emitMsg mayCtx msg = ts.updateState
      ( \(FullState state) -> case updatePubState msg state.pubState of
          Left err -> do
            log err
            pure (FullState state)
          Right newState -> do
            let { tag, args } = tagName msg

            logJson
              ( [ encodeJson ("%c" <> tag)
                , encodeJson "color: white; background: #cc8a21; padding: 2px 4px;"
                ] <> (maybe [] (\v -> [ encodeJson ("@" <> v) ]) mayCtx) <>
                  [ args
                  , encodeJson "\nnewState"
                  , encodeJsonPubState newState
                  ]
              )

            pure
              ( state
                  # set (prop @"pubState") newState
                  # set (prop @"historyIndex") (state.historyIndex + 1.0)
                  # over (prop @"history") (\xs -> xs <> [ { msg, pubState: newState } ])
                  # FullState
              )
      )

---

moduleName :: String
moduleName = "GCodeViewer.Lib"

tsExports :: Either TSB.AppError (Array DTS.TsModuleFile)
tsExports = TSB.tsModuleFile moduleName
  [ TSB.tsValues Tok
      {
      }
  ]

foreign import logJson :: Array Json -> Effect Unit