module GCodeViewer.State where

import Prelude

import Affjax as Affjax
import Affjax.ResponseFormat (json)
import Affjax.StatusCode (StatusCode(..))
import Affjax.Web as AffjaxWeb
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except (Except, ExceptT, runExcept, runExceptT)
import Control.Monad.Writer (Writer)
import DTS as DTS
import Data.Argonaut.Core (stringify)
import Data.Array as Array
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, Fiber, delay, launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import GCodeViewer.Lib (MkAppState, DispatcherApi, TsApi, tsApiToDispatcherApi)
import GCodeViewer.Lib as Lib
import GCodeViewer.TsBridge (class TsBridge, Tok(..))
import TsBridge (tsTypeAlias)
import TsBridge as TSB
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- Public state

type AppState = Lib.MkAppState PubState PrivState

type PubState =
  { items :: Array Item
  , errors :: Array String
  }

type IndexFile =
  Array IndexFileItem

type IndexFileItem =
  { name :: String
  , gcode :: String
  , pictures :: Array String
  }

codecIndexFileItem :: JsonCodec IndexFileItem
codecIndexFileItem = CAR.object "IndexFileItem"
  { name: CA.string
  , gcode: CA.string
  , pictures: CA.array CA.string
  }

type Item =
  { file :: IndexFileItem
  , lines :: Maybe (Array String)
  , startLayer :: Int
  , endLayer :: Int
  }

mkItem :: IndexFileItem -> Item
mkItem file =
  { file
  , lines: Nothing
  , startLayer: 0
  , endLayer: 0
  }

initPubState :: PubState
initPubState =
  { items: []
  , errors: []
  }

data Msg
  = MsgSetItems (Array Item)
  | MsgLoadLines { itemIndex :: Int, lines :: Array String }
  | MsgChangeStartLayer { itemIndex :: Int, startLayer :: Int }
  | MsgChangeEndLayer { itemIndex :: Int, endLayer :: Int }
  | MsgAddError String

updatePubState :: Msg -> PubState -> Except String PubState
updatePubState msg pubState = case msg of
  MsgChangeStartLayer { itemIndex, startLayer } ->
    updateItem itemIndex (\item -> item { startLayer = startLayer }) pubState

  MsgChangeEndLayer { itemIndex, endLayer } ->
    updateItem itemIndex (\item -> item { endLayer = endLayer }) pubState

  MsgSetItems items ->
    pure $ pubState { items = items }

  MsgLoadLines { itemIndex, lines } ->
    updateItem itemIndex (\item -> item { lines = Just lines }) pubState

  MsgAddError error -> pure $ pubState { errors = [ error ] }

updateItem :: Int -> (Item -> Item) -> PubState -> Except String PubState
updateItem itemIndex f pubState = case Array.modifyAt itemIndex f pubState.items of
  Just items -> pure $ pubState { items = items }
  Nothing -> throwError "Item not found"

-- Priv state

newtype PrivState = PrivState {}

initPrivState :: PrivState
initPrivState = PrivState {}

-- State

initialState :: AppState
initialState = Lib.MkAppState
  { pubState: initPubState
  , privState: initPrivState
  }

---

type Dispatchers =
  { fetchIndexFile :: Effect Unit
  , loadLines :: { itemIndex :: Int, lines :: Array String } -> Effect Unit
  , changeStartLayer :: { itemIndex :: Int, startLayer :: Int } -> Effect Unit
  , changeEndLayer :: { itemIndex :: Int, endLayer :: Int } -> Effect Unit
  }

catchByEmit :: { emitMsg :: Msg -> Effect Unit } -> ExceptT String Aff Unit -> Effect Unit
catchByEmit { emitMsg } act = launchAff_ do
  result <- runExceptT act
  case result of
    Left err -> do
      liftEffect $ emitMsg (MsgAddError err)
      log ("error: " <> err)
    Right _ -> pure unit

handleAffEither :: forall m err a. MonadError String m => MonadAff m => (err -> String) -> Aff (Either err a) -> m a
handleAffEither errToString act = do
  ret <- liftAff act
  case ret of
    Left err -> throwError $ errToString err
    Right a -> pure a

handleEither :: forall m err a. MonadError String m => (err -> String) -> Either err a -> m a
handleEither errToString = case _ of
  Left err -> throwError $ errToString err
  Right a -> pure a

getIndexFile :: ExceptT String Aff IndexFile
getIndexFile = do
  ret <- handleAffEither Affjax.printError $ AffjaxWeb.get json "/out/index.json"

  when (ret.status /= StatusCode 200) $ throwError "Failed to get index file"

  val <- handleEither CA.printJsonDecodeError $ CA.decode (CA.array codecIndexFileItem) ret.body

  pure val

dispatchers :: DispatcherApi Msg PubState PrivState -> Dispatchers
dispatchers { emitMsg } =
  { fetchIndexFile: catchByEmit { emitMsg } do
      liftAff $ delay (Milliseconds 1000.0)

      ret <- getIndexFile

      liftEffect $ emitMsg (MsgSetItems (map mkItem ret))

  , loadLines: \n -> emitMsg (MsgLoadLines n)

  , changeStartLayer: \n -> emitMsg (MsgChangeStartLayer n)

  , changeEndLayer: \n -> emitMsg (MsgChangeEndLayer n)
  }

---

tsDispatchers :: TsApi AppState -> Dispatchers
tsDispatchers tsApi = dispatchers (tsApiToDispatcherApi updatePubState' tsApi)
  where
  updatePubState' msg pubState = runExcept (updatePubState msg pubState)

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
  , tsTypeAlias Tok "Item" (Proxy :: _ Item)
  ]