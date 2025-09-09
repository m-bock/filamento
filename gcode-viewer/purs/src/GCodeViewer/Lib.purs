module GCodeViewer.Lib
  ( MkAppState
  , mkTsApi
  , DispatcherApi
  , TsApi(..)
  , TsStateHandle
  , tsExports
  ) where

import Prelude

import DTS as DTS
import Data.Either (Either(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console (log)
import GCodeViewer.TsBridge (class TsBridge, Tok(..), tsBridge)
import TsBridge (TypeVar)
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
  , updatePrivState :: (privState -> privState) -> Effect Unit
  }

getPubState :: forall pubState privState. MkAppState pubState privState -> pubState
getPubState (MkAppState state) = state.pubState

---

type PursConfig msg pubState privState err disp =
  { updatePubState :: msg -> pubState -> Either String pubState
  , dispatchers :: DispatcherApi msg pubState privState -> disp
  , printError :: err -> String
  , initPubState :: pubState
  , initPrivState :: privState
  }

type TsStateHandle state =
  { updateState :: (state -> Effect state) -> Effect Unit
  , readState :: Effect state
  }

newtype TsApi pubState state disp = TsApi
  { dispatchers :: TsStateHandle state -> disp
  , initState :: state
  , getPubState :: state -> pubState
  }

derive instance Newtype (TsApi pubState state disp) _

instance (TsBridge pubState, TsBridge state, TsBridge disp) => TsBridge (TsApi pubState state disp) where
  tsBridge = TSB.tsBridgeNewtype Tok
    { moduleName
    , typeName: "TsApi"
    , typeArgs:
        [ "pubState" /\ tsBridge (Proxy :: _ pubState)
        , "state" /\ tsBridge (Proxy :: _ state)
        , "disp" /\ tsBridge (Proxy :: _ disp)
        ]
    }

mkTsApi :: forall msg pubState privState err disp. PursConfig msg pubState privState err disp -> TsApi pubState (MkAppState pubState privState) disp
mkTsApi { initPubState, initPrivState, dispatchers, updatePubState } =
  TsApi
    { dispatchers: f >>> dispatchers
    , initState: MkAppState { pubState: initPubState, privState: initPrivState }
    , getPubState
    }
  where
  f :: TsStateHandle (MkAppState pubState privState) -> DispatcherApi msg pubState privState
  f ts =
    { emitMsg: \msg -> ts.updateState
        ( \(MkAppState state) -> case updatePubState msg state.pubState of
            Left err -> do
              log err
              pure $ MkAppState state
            Right newState -> pure $ MkAppState (state { pubState = newState })
        )
    , readPubState: do
        MkAppState st <- ts.readState
        pure st.pubState
    , readPrivState: do
        MkAppState st <- ts.readState
        pure st.privState
    , updatePrivState: \f ->
        ts.updateState (\(MkAppState state) -> pure $ MkAppState (state { privState = f state.privState }))
    }

---

moduleName :: String
moduleName = "GCodeViewer.Lib"

instance (TsBridge pubState, TsBridge privState) => TsBridge (MkAppState pubState privState) where
  tsBridge = TSB.tsBridgeOpaqueType
    { moduleName
    , typeName: "MkAppState"
    , typeArgs:
        [ "pubState" /\ tsBridge (Proxy :: _ pubState)
        , "privState" /\ tsBridge (Proxy :: _ privState)
        ]
    }

tsExports :: Either TSB.AppError (Array DTS.TsModuleFile)
tsExports = TSB.tsModuleFile moduleName
  [ TSB.tsValues Tok
      {
      }
  --, TSB.tsTypeAlias Tok "TsApi" (Proxy :: _ (TsApi (TypeVar "A") (TypeVar "B") (TypeVar "C")))
  ]