module GCodeViewer.Lib
  ( FullState
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

type FullState pubState privState =
  { pubState :: pubState
  , privState :: privState
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
  }

type TsStateHandle state =
  { updateState :: (state -> Effect state) -> Effect Unit
  , readState :: Effect state
  }

newtype TsApi pubState privState disp = TsApi
  { dispatchers :: TsStateHandle (FullState pubState privState) -> disp
  , initState :: FullState pubState privState
  }

derive instance Newtype (TsApi pubState state disp) _

instance (TsBridge pubState, TsBridge privState, TsBridge disp) => TsBridge (TsApi pubState privState disp) where
  tsBridge = TSB.tsBridgeNewtype Tok
    { moduleName
    , typeName: "TsApi"
    , typeArgs:
        [ "pubState" /\ tsBridge (Proxy :: _ pubState)
        , "privState" /\ tsBridge (Proxy :: _ privState)
        , "disp" /\ tsBridge (Proxy :: _ disp)
        ]
    }

mkTsApi :: forall msg pubState privState err disp. PursConfig msg pubState privState err disp -> TsApi pubState privState disp
mkTsApi { initPubState, initPrivState, dispatchers, updatePubState } =
  TsApi
    { dispatchers: f >>> dispatchers
    , initState: { pubState: initPubState, privState: initPrivState }
    }
  where
  f :: TsStateHandle (FullState pubState privState) -> DispatcherApi msg pubState privState
  f ts =
    { emitMsg: \msg -> ts.updateState
        ( \state -> case updatePubState msg state.pubState of
            Left err -> do
              log err
              pure state
            Right newState -> pure (state { pubState = newState })
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