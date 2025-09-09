module GCodeViewer.StateMachine where

import GCodeViewer.Prelude

import DTS as DTS
import Data.Newtype (class Newtype)
import GCodeViewer.Error (Err, printErr)
import GCodeViewer.Lib (DispatcherApi, MkAppState, TsApi, mkTsApi)
import GCodeViewer.TsBridge (class TsBridge, Tok(..))
import TsBridge as TSB

newtype PubState = PubState
  { x :: Int }

derive instance Newtype PubState _

instance TsBridge PubState where
  tsBridge = TSB.tsBridgeNewtype Tok
    { moduleName
    , typeName: "PubState"
    , typeArgs: []
    }

initPubState :: PubState
initPubState = PubState
  { x: 0 }

updatePubState :: Msg -> PubState -> Except String PubState
updatePubState msg pubState = pure pubState

data Msg = Msg1 | Msg2 | Msg3

runSafe :: ExceptT Err Aff Unit -> Effect Unit
runSafe act = launchAff_ do
  result <- runExceptT act
  case result of
    Left err -> log (printErr err)
    Right _ -> pure unit

dispatchers :: DispatcherApi Msg PubState {} -> _
dispatchers { emitMsg, readPubState } =
  { disp1: emitMsg Msg1
  , disp2: emitMsg Msg2
  , disp3: emitMsg Msg3
  , disp4: runSafe disp4
  }
  where
  disp4 :: ExceptT Err Aff Unit
  disp4 = do
    liftAff $ delay (Milliseconds 1000.0)
    st <- liftEffect $ readPubState
    liftEffect $ emitMsg Msg1

    liftAff $ delay (Milliseconds 1000.0)
    liftEffect $ emitMsg Msg2

    liftAff $ delay (Milliseconds 1000.0)
    liftEffect $ emitMsg Msg3

    pure unit

tsApi :: TsApi PubState (MkAppState PubState {}) _
tsApi = mkTsApi
  { updatePubState: \msg s -> runExcept (updatePubState msg s)
  , dispatchers
  , initPubState
  , initPrivState: {}
  , printError: identity
  }

moduleName :: String
moduleName = "GCodeViewer.StateMachine"

tsExports :: Either TSB.AppError (Array DTS.TsModuleFile)
tsExports = TSB.tsModuleFile moduleName
  [ TSB.tsValues Tok
      { tsApi
      }
  ]