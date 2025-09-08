module GCodeViewer.Lib where

import Prelude

import DTS as DTS
import Data.Either (Either)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import GCodeViewer.TsBridge (class TsBridge, Tok(..), tsBridge)
import TsBridge as TSB
import Type.Prelude (Proxy(..))

-- Public state

newtype MkAppState pubState privState = MkAppState
  { pubState :: pubState
  , privState :: privState
  }

type DispatcherApi msg pubState privState =
  { emitMsg :: msg -> Effect Unit
  , readPubState :: Effect pubState
  , readPrivState :: Effect privState
  , updatePubState :: (pubState -> pubState) -> Effect Unit
  }

getPubState :: forall pubState privState. MkAppState pubState privState -> pubState
getPubState (MkAppState state) = state.pubState

---

type TsApi state =
  { updateState :: (state -> state) -> Effect Unit
  , readState :: Effect state
  }

tsApiToDispatcherApi :: forall msg pubState privState. (msg -> pubState -> pubState) -> TsApi (MkAppState pubState privState) -> DispatcherApi msg pubState privState
tsApiToDispatcherApi updatePubState { updateState, readState } =
  { emitMsg: \msg ->
      updateState (\(MkAppState state) -> MkAppState (state { pubState = updatePubState msg state.pubState }))

  , readPubState: do
      MkAppState st <- readState
      pure st.pubState

  , readPrivState: do
      MkAppState st <- readState
      pure st.privState

  , updatePubState: \f ->
      updateState (\(MkAppState state) -> MkAppState (state { pubState = f state.pubState }))
  }

---

moduleName :: String
moduleName = "GCodeViewer.Lib"

instance (TsBridge pubState, TsBridge privState) => TsBridge (MkAppState pubState privState) where
  tsBridge = TSB.tsBridgeOpaqueType
    { moduleName
    , typeName: "AppState"
    , typeArgs: [ "pubState" /\ tsBridge (Proxy :: _ pubState), "privState" /\ tsBridge (Proxy :: _ privState) ]
    }

tsExports :: Either TSB.AppError (Array DTS.TsModuleFile)
tsExports = TSB.tsModuleFile moduleName
  [ TSB.tsValues Tok
      {
      }
  ]