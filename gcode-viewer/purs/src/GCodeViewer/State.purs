module GCodeViewer.State where

import Prelude

-- import Affjax as Affjax
-- import Affjax.ResponseFormat (json, string)
-- import Affjax.StatusCode (StatusCode(..))
-- import Affjax.Web as AffjaxWeb
-- import Control.Monad.Error.Class (class MonadError, catchError, throwError)
-- import Control.Monad.Except (Except, ExceptT, runExcept, runExceptT)
-- import Control.Monad.Writer (Writer)
-- import DTS as DTS
-- import Data.Argonaut.Core (stringify)
-- import Data.Array as Array
-- import Data.Codec.Argonaut (JsonCodec)
-- import Data.Codec.Argonaut as CA
-- import Data.Codec.Argonaut.Record as CAR
-- import Data.Either (Either(..))
-- import Data.Int as Int
-- import Data.Lens (over, set)
-- import Data.Lens.Record (prop)
-- import Data.Map.Internal (unsafeBalancedNode)
-- import Data.Maybe (Maybe(..), fromMaybe)
-- import Data.Time.Duration (Milliseconds(..))
-- import Data.Traversable (for_)
-- import Data.Tuple.Nested ((/\))
-- import Effect (Effect)
-- import Effect.Aff (Aff, Fiber, delay, launchAff_)
-- import Effect.Aff.Class (class MonadAff, liftAff)
-- import Effect.Class (liftEffect)
-- import Effect.Class.Console (log)
-- import GCodeViewer.Lib (MkAppState, DispatcherApi, TsApi, tsApiToDispatcherApi)
-- import GCodeViewer.Lib as Lib
-- import GCodeViewer.TsBridge (class TsBridge, Tok(..), tsBridge)
-- import TsBridge (tsTypeAlias)
-- import TsBridge as TSB
-- import Type.Prelude (Proxy(..))
-- import Unsafe.Coerce (unsafeCoerce)

-- Public state

-- type AppState = Lib.MkAppState PubState PrivState

-- type PubState =
--   { items :: RemoteData (Array Item)
--   , errors :: Array String
--   }

-- type RemoteData a =
--   { value :: a
--   , status :: RemoteDataStatus
--   }

-- data RemoteDataStatus
--   = NotAsked
--   | Loading
--   | Loaded
--   | Error String

-- mkRemoteDataStatus
--   :: { notAsked :: RemoteDataStatus
--      , loading :: RemoteDataStatus
--      , loaded :: RemoteDataStatus
--      , error :: String -> RemoteDataStatus
--      }
-- mkRemoteDataStatus =
--   { notAsked: NotAsked
--   , loading: Loading
--   , loaded: Loaded
--   , error: Error
--   }

-- onRemoteDataStatus
--   :: forall @z
--    . { notAsked :: z
--      , loading :: z
--      , loaded :: z
--      , error :: String -> z
--      }
--   -> RemoteDataStatus
--   -> z
-- onRemoteDataStatus = \{ notAsked, loading, loaded, error } rd -> case rd of
--   NotAsked -> notAsked
--   Loading -> loading
--   Loaded -> loaded
--   Error err -> error err

-- instance TsBridge RemoteDataStatus where
--   tsBridge = TSB.tsBridgeOpaqueType
--     { moduleName
--     , typeName: "RemoteDataStatus"
--     , typeArgs: []
--     }

-- derive instance Eq RemoteDataStatus

-- type Item =
--   { file :: IndexFileItem
--   , lines :: Maybe (Array String)
--   , startLayer :: Int
--   , endLayer :: Int
--   }

-- mkItem :: IndexFileItem -> Item
-- mkItem file =
--   { file
--   , lines: Nothing
--   , startLayer: 0
--   , endLayer: 0
--   }

-- initPubState :: PubState
-- initPubState =
--   { items: { value: [], status: NotAsked }
--   , errors: []
--   }

-- data Msg
--   = MsgLoadLines { itemIndex :: Int, lines :: Array String }
--   | MsgChangeStartLayer { itemIndex :: Int, startLayer :: Int }
--   | MsgChangeEndLayer { itemIndex :: Int, endLayer :: Int }
--   | MsgAddError String
--   | MsgSetItems { value :: Maybe (Array Item), status :: RemoteDataStatus }

-- updatePubState :: Msg -> PubState -> Except String PubState
-- updatePubState msg pubState = case msg of
--   MsgChangeStartLayer { itemIndex, startLayer } ->
--     updateItem itemIndex (\item -> item { startLayer = startLayer }) pubState

--   MsgChangeEndLayer { itemIndex, endLayer } ->
--     updateItem itemIndex (\item -> item { endLayer = endLayer }) pubState

--   MsgSetItems items -> unsafeCoerce ""
--   -- pure $ pubState { items = items }

--   MsgLoadLines { itemIndex, lines } ->
--     updateItem itemIndex (\item -> item { lines = Just lines }) pubState

--   MsgAddError error -> pure $ pubState { errors = [ error ] }

--   MsgSetItems { value, status } -> do --pure $ pubState { items = { fromMaybe  value, status } }

--     case value of
--       Just items -> pure $ set (prop (Proxy :: _ "items") <<< prop (Proxy :: _ "value")) items pubState
--       Nothing -> pure unit

--     pure $ over (prop (Proxy :: _ "items") <<< prop (Proxy :: _ "status")) status pubState

-- updateItem :: Int -> (Item -> Item) -> PubState -> Except String PubState
-- updateItem itemIndex f pubState = unsafeCoerce ""

-- --  case Array.modifyAt itemIndex f pubState.items of
-- --   Just items -> pure $ pubState { items = items }
-- --   Nothing -> throwError "Item not found"

-- -- Priv state

-- newtype PrivState = PrivState {}

-- initPrivState :: PrivState
-- initPrivState = PrivState {}

-- -- State

-- initialState :: AppState
-- initialState = Lib.MkAppState
--   { pubState: initPubState
--   , privState: initPrivState
--   }

-- ---

-- catchByEmit :: { emitError :: String -> Effect Unit } -> ExceptT String Aff Unit -> Effect Unit
-- catchByEmit { emitError } act = launchAff_ do
--   result <- runExceptT act
--   case result of
--     Left err -> do
--       liftEffect $ emitError err
--       log ("error: " <> err)
--     Right _ -> pure unit

-- dispatchers :: DispatcherApi Msg PubState PrivState -> _
-- dispatchers { emitMsg, readPubState } =
--   { fetchIndexFile
--   , fetchAllFiles
--   , changeStartLayer
--   , changeEndLayer
--   }

--   where
--   changeStartLayer :: { itemIndex :: Int, startLayer :: Int } -> Effect Unit
--   changeStartLayer n = emitMsg (MsgChangeStartLayer n)

--   changeEndLayer :: { itemIndex :: Int, endLayer :: Int } -> Effect Unit
--   changeEndLayer n = emitMsg (MsgChangeEndLayer n)

--   fetchIndexFile :: Effect Unit
--   fetchIndexFile = catchByEmit
--     { emitError: \err -> do
--         emitMsg (MsgAddError err)
--     --emitMsg (MsgSetItems (Error err))
--     }
--     do
--       --liftEffect $ emitMsg (MsgSetItems Loading)
--       liftAff $ delay (Milliseconds 1000.0)

--       ret <- getIndexFile

--       pure unit

--   --liftEffect $ emitMsg (MsgSetItems (Loaded $ map mkItem ret))

--   fetchAllFiles :: Effect Unit
--   fetchAllFiles = catchByEmit
--     { emitError: \err -> emitMsg (MsgAddError err)
--     }
--     do
--       st <- liftEffect $ readPubState

--       pure unit

-- -- for_ st.items \item -> do
-- --   liftAff $ delay (Milliseconds 1000.0)

-- --   -- ret <- getGCodeFile item.file.gcode

-- --   pure unit

-- ---

-- tsDispatchers :: TsApi AppState -> _
-- tsDispatchers tsApi = dispatchers (tsApiToDispatcherApi updatePubState' tsApi)
--   where
--   updatePubState' msg pubState = runExcept (updatePubState msg pubState)

-- intToNumber :: Int -> Number
-- intToNumber = Int.toNumber

-- round :: Number -> Int
-- round = Int.round

-- getPubState :: AppState -> PubState
-- getPubState (Lib.MkAppState state) = state.pubState

-- ---

-- eqPubState :: PubState -> PubState -> Boolean
-- eqPubState a b = a == b

-- moduleName :: String
-- moduleName = "GCodeViewer.State"

-- instance TsBridge Msg where
--   tsBridge = TSB.tsBridgeOpaqueType
--     { moduleName
--     , typeName: "Msg"
--     , typeArgs: []
--     }

-- -- --

-- instance TsBridge PrivState where
--   tsBridge = TSB.tsBridgeOpaqueType
--     { moduleName
--     , typeName: "PrivState"
--     , typeArgs: []
--     }

-- tsExports :: Either TSB.AppError (Array DTS.TsModuleFile)
-- tsExports = TSB.tsModuleFile moduleName
--   [ TSB.tsValues Tok
--       { initialState
--       , tsDispatchers
--       , getPubState
--       , eqPubState
--       , intToNumber
--       , round
--       , mkRemoteDataStatus
--       , onRemoteDataStatus: onRemoteDataStatus @String
--       }
--   , tsTypeAlias Tok "AppState" (Proxy :: _ AppState)
--   , tsTypeAlias Tok "PubState" (Proxy :: _ PubState)
--   --, tsTypeAlias Tok "Dispatchers" (Proxy :: _ Dispatchers)
--   , tsTypeAlias Tok "Item" (Proxy :: _ Item)
--   ]